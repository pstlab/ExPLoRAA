let user;
let ws;
const config = {
    'host': '192.168.1.101',
    'service_port': 7000,
    'websocket_port': 8884
};

$(window).on('load', function () {
    const email = localStorage.getItem('email');
    const password = localStorage.getItem('password');
    if (email && password) {
        const form = new FormData();
        form.append('email', email);
        form.append('password', password);
        fetch('http://' + config.host + ':' + config.service_port + '/login', {
            method: 'post',
            body: form
        }).then(response => {
            if (response.ok) {
                response.json().then(data => { setUser(data); });
            } else {
                localStorage.removeItem('email');
                localStorage.removeItem('password');
                $.get('body_guest.html', function (data) { $('#exploraa-body').append(data); });
            }
        });
    } else {
        $.get('body_guest.html', function (data) { $('#exploraa-body').append(data); });
    }
});

function login() {
    const email = $('#login-email').val();
    const password = $('#login-password').val();
    const form = new FormData();
    form.append('email', email);
    form.append('password', password);
    fetch('http://' + config.host + ':' + config.service_port + '/login', {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            location.reload();
        } else
            alert(response.statusText);
    });
}

function logout() {
    localStorage.removeItem('email');
    localStorage.removeItem('password');
    location.reload();
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
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            location.reload();
        } else
            alert(response.statusText);
    });
}

function delete_user() {
    fetch('http://' + config.host + ':' + config.service_port + '/users/' + user.id, {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            logout();
        } else
            alert(response.statusText);
    });
}

function update_user() {
    user.firstName = $('#profile-first-name').val();
    user.lastName = $('#profile-last-name').val();
    fetch('http://' + config.host + ':' + config.service_port + '/users/' + user.id, {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + user.id },
        body: JSON.stringify(user)
    }).then(response => {
        if (response.ok) {
            $('#account-menu').text(user.firstName);
            $('#profile-modal').modal('hide');
        } else
            alert(response.statusText);
    });
}

function setUser(usr) {
    user = usr;
    $('#body-guest').remove();
    $.get('body_user.html', function (data) {
        $('#exploraa-body').append(data);
        $('#account-menu').text(user.firstName);
        $('#profile-email').val(user.email);
        $('#profile-first-name').val(user.firstName);
        $('#profile-last-name').val(user.lastName);

        for (const [key, value] of Object.entries(user.teachers).sort((a, b) => (a.teacher.lastName + a.teacher.firstName).localeCompare(b.teacher.lastName + b.teacher.firstName))) {
            console.log(value);
            $('#f-teachers-list').append(`
            <div class="list-group-item list-group-item-action d-flex justify-content-between align-items-center">
                <div class="col d-flex justify-content-start align-items-center"><span class="fas fa-link mr-1"></span>${value.teacher.lastName}, ${value.teacher.firstName}</div>
                <div class="col d-flex justify-content-end"><a role="button" class="btn btn-sm btn-secondary"><i class="fas fa-user-minus"></i></a></div>
            </div>
            `);
        }

        ws = new WebSocket('ws://' + config.host + ':' + config.service_port + '/communication/?id=' + user.id, 'exploraa-ws');
        ws.onmessage = msg => {
            const c_msg = JSON.parse(msg);
            switch (c_msg.type) {
                case "online":
                    break;
                case "follower":
                    break;
                default:
                    break;
            }
            console.log(msg);
        };
    });
}

function show_teachers() {
    $('#teachers-list').empty();

    fetch('http://' + config.host + ':' + config.service_port + '/users', {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.filter(teacher => teacher.id != user.id && !(teacher.id in user.teachers)).forEach(teacher => {
                    $('#teachers-list').append(`
                    <div class="list-group-item list-group-item-action custom-control custom-checkbox">
                        <input id="teacher-${teacher.id}" type="checkbox" teacher_id="${teacher.id}">
                        <label for="teacher-${teacher.id}">${teacher.lastName}, ${teacher.firstName}</label>
                    </div>
                    `);
                });
                $('#show-teachers-modal').modal('show');
            });
        } else
            alert(response.statusText);
    });
}

function follow_teachers() {
    $('#teachers-list').find('input:checked').each(function () {
        fetch('http://' + config.host + ':' + config.service_port + '/follow/?student_id=' + user.id + '&teacher_id=' + this.getAttribute('teacher_id'), {
            method: 'post',
            headers: { 'Authorization': 'Basic ' + user.id }
        }).then(response => {
            if (!response.ok)
                alert(response.statusText);
        });
    });
}