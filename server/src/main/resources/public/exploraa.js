$(document).ready(function () {
    var email = getCookie("email");
    var password = getCookie("password");
    if (email != "" && password != "") {
        fetch("http://localhost/login", {
            method: 'post',
            body: new URLSearchParams({ "email": email, "password": password }),
            headers: new Headers({ 'Content-type': 'application/x-www-form-urlencoded; charset=UTF-8' })
        }).then(response => {
            console.log(response);
            setCookie("email", email, 30);
            setCookie("password", password, 30);
        }).catch(error => console.log("Si è verificato un errore!"))
    }
});

function setCookie(cname, cvalue, exdays) {
    var d = new Date();
    d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
    var expires = "expires=" + d.toUTCString();
    document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for (var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length, c.length);
        }
    }
    return "";
}