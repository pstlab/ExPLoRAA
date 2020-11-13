let user;
const config = {
    'host': '192.168.1.101',
    "service_port": 7000,
    "websocket_port": 8884
};

function login() {
    const email = $('#login-email').val();
    const password = $('#login-password').val();
    let form = new FormData();
    form.append('email', email);
    form.append('password', password);
    fetch('http://' + config.host + ':' + config.service_port + '/login', {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $('#navbar-content').remove();
            $('#signin-form').remove();
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            response.json().then(data => { setUser(data); });
        } else
            alert(response.statusText);
    });
}

function signin() {
    const email = $('#signin-email').val();
    const password = $('#signin-password').val();
    const first_name = $('#signin-first-name').val();
    const last_name = $('#signin-last-name').val();
    const form = new FormData();
    form.append('email', email);
    form.append('password', password);
    form.append('first_name', first_name);
    form.append('last_name', last_name);
    form.append('email', email);
    form.append('password', password);
    form.append('first_name', first_name);
    form.append('last_name', last_name);
    fetch('http://' + config.host + ':' + config.service_port + '/users', {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $('#navbar-content').remove();
            $('#signin-form').remove();
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            location.reload();
        } else
            alert(response.statusText);
    });
}