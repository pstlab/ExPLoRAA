$(window).on("load", function () {
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
        } else {
            $.get("login_form.html", function (data) { $("#nav-bar").append(data); });
            $.get("signin_form.html", function (data) { $("#body").append(data); });
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
    $("#navbar-content").remove();
    $.get("login_form.html", function (data) { $("#nav-bar").append(data); });
    $.get("signin_form.html", function (data) { $("#body").append(data); });
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

function setUser(user) {
    console.log(user);
    $("#navbar-content").remove();
    $("#signin-form").remove();
    $.get("nav_bar.html", function (data) {
        $("#nav-bar").append(data);
        if (user.firstName) {
            $("#account-menu").text(user.firstName);
        }
    });
}