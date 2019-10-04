$(document).ready(function () {
    var user = sessionStorage.getItem("user");
    if (user) {
        setUser(user);
    } else {
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
                console.log(response);
                if (response.ok) {
                    localStorage.setItem("email", email);
                    localStorage.setItem("password", password);
                    response.json().then(data => {
                        setUser(data);
                        sessionStorage.setItem("user", JSON.stringify(data));
                    });
                } else {
                    localStorage.removeItem("email");
                    localStorage.removeItem("password");
                }
            }).catch(error => {
                console.log(error.json());
            });
        }
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
        console.log(response);
        if (response.ok) {
            localStorage.setItem("email", email);
            localStorage.setItem("password", password);
            response.json().then(data => {
                setUser(data);
                sessionStorage.setItem("user", JSON.stringify(data));
            });
        } else {
            localStorage.removeItem("email");
            localStorage.removeItem("password");
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function logout() {
    sessionStorage.removeItem("user");
    localStorage.removeItem("email");
    localStorage.removeItem("password");
    $.get("login_form.html", function (data) {
        $("#login-form").append(data);
    });
    $.get("signin_form.html", function (data) {
        $("#signin-form").append(data);
    });
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
        console.log(response);
        if (response.ok) {
            setCookie("email", email, 30);
            setCookie("password", password, 30);
            response.json().then(data => {
                setUser(data);
                sessionStorage.setItem("user", JSON.stringify(data));
            });
        } else {
            setCookie("email", email, -1);
            setCookie("password", password, -1);
        }
    }).catch(error => {
        console.log(error.json());
    });
}

function setUser(user) {
    $("#login-form").remove();
    $("#signin-form").remove();
}