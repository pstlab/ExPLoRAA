let user;

function login() {
    const email = $('#login-email').val();
    const password = $('#login-password').val();
    let form = new FormData();
    form.append('email', email);
    form.append('password', password);
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
}