let user;
let mqtt_client;

$(window).on("load", function () {
    let email = localStorage.getItem("email");
    let password = localStorage.getItem("password");
    if (email && password) {
        let form = new FormData();
        form.append("email", email);
        form.append("password", password);
        fetch("http://localhost:7000/login", {
            method: 'post',
            body: form
        }).then(response => {
            if (response.ok) {
                response.json().then(data => { setUser(data); });
            } else {
                localStorage.removeItem("email");
                localStorage.removeItem("password");
                $.get("login_form.html", function (data) { $("#nav-bar").append(data); });
                $.get("signin_form.html", function (data) { $("#body").append(data); });
            }
        }).catch(error => {
            console.log(error.json());
        });
    } else {
        $.get("login_form.html", function (data) { $("#nav-bar").append(data); });
        $.get("signin_form.html", function (data) { $("#body").append(data); });
    }
});

function login() {
    let email = $("#login-email").val();
    let password = $("#login-password").val();
    let form = new FormData();
    form.append("email", email);
    form.append("password", password);
    fetch("http://localhost:7000/login", {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $("#navbar-content").remove();
            $("#signin-form").remove();
            localStorage.setItem("email", email);
            localStorage.setItem("password", password);
            response.json().then(data => { setUser(data); });
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function logout() {
    mqtt_client.disconnect();
    localStorage.removeItem("email");
    localStorage.removeItem("password");
    location.reload(false);
}

function signin() {
    let email = $("#signin-email").val();
    let password = $("#signin-password").val();
    let first_name = $("#signin-first-name").val();
    let last_name = $("#signin-last-name").val();
    let form = new FormData();
    form.append("email", email);
    form.append("password", password);
    form.append("first_name", first_name);
    form.append("last_name", last_name);
    fetch("http://localhost:7000/users", {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $("#navbar-content").remove();
            $("#signin-form").remove();
            localStorage.setItem("email", email);
            localStorage.setItem("password", password);
            location.reload(false);
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function deleteUser() {
    fetch("http://localhost:7000/users/" + user.id, {
        method: 'delete'
    }).then(response => {
        if (response.ok) {
            mqtt_client.disconnect();
            localStorage.removeItem("email");
            localStorage.removeItem("password");
            location.reload(false);
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function setUser(usr) {
    user = usr;
    $.get("nav_bar.html", function (data) {
        $("#nav-bar").append(data);
        if (user.firstName) {
            $("#account-menu").text(user.firstName);
        }

        $("#profile").on("show.bs.modal", function (event) {
            $("#e-mail").val(user.email);
            $("#first-name").val(user.firstName);
            $("#last-name").val(user.lastName);
            $("#save-profile").click(function () {
                user.firstName = $("#first-name").val();
                user.lastName = $("#last-name").val();
                fetch("http://localhost:7000/users/" + user.id, {
                    method: 'patch',
                    body: JSON.stringify(user)
                }).then(response => {
                    if (response.ok) {
                        if (user.firstName) {
                            $("#account-menu").text(user.firstName);
                        }
                        $("#profile").modal("hide");
                    }
                }).catch(error => {
                    console.log(error.json());
                });
            });
        })
        $.get("body.html", function (data) {
            $("#body").append(data);
        });
    });

    // Create a client instance
    mqtt_client = new Paho.MQTT.Client("localhost", 8080, "user" + user.id);

    // set callback handlers
    // called when the client loses its connection
    mqtt_client.onConnectionLost = function (responseObject) {
        if (responseObject.errorCode !== 0) {
            console.log("onConnectionLost:" + responseObject.errorMessage);
        }
        user.online = false;
    };
    // called when a message arrives
    mqtt_client.onMessageArrived = function (message) {
        console.log("onMessageArrived:" + message.payloadString);
    };

    // connect the client
    mqtt_client.connect({
        onSuccess: function () { // called when the client connects
            // Once a connection has been made, make a subscription and send a message.
            console.log("onConnect");
            user.online = true;
            mqtt_client.subscribe("ExPLoRAA/" + user.id + "/#", { qos: 2 });
            message = new Paho.MQTT.Message("Hello");
            message.destinationName = "ExPLoRAA/" + user.id;
            mqtt_client.send(message);
        }
    });
}