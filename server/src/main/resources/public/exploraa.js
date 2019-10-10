var user;
var mqtt_client;

$(window).on("load", function () {
    var email = localStorage.getItem("email");
    var password = localStorage.getItem("password");
    if (email && password) {
        var form = new FormData();
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
    var email = $("#login-email").val();
    var password = $("#login-password").val();
    var form = new FormData();
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
        } else {
            localStorage.removeItem("email");
            localStorage.removeItem("password");
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function logout() {
    mqtt_client.disconnect();
    localStorage.removeItem("email");
    localStorage.removeItem("password");
    $("#navbar-content").remove();
    $.get("login_form.html", function (data) { $("#nav-bar").append(data); });
    $.get("signin_form.html", function (data) { $("#body").append(data); });
    user = undefined;
}

function signin() {
    var email = $("#signin-email").val();
    var password = $("#signin-password").val();
    var form = new FormData();
    form.append("email", email);
    form.append("password", password);
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
        } else {
            localStorage.removeItem("email");
            localStorage.removeItem("password");
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
                var url = "http://localhost:7000/users/" + user.id;
                fetch(url, {
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
            mqtt_client.subscribe("World", { qos: 2 });
            message = new Paho.MQTT.Message("Hello");
            message.destinationName = "World";
            mqtt_client.send(message);
        }
    });
}