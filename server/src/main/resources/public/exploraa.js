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
            $('#f-teachers-list').append(`
            <div class="list-group-item list-group-item-action d-flex justify-content-between align-items-center" id="f-teacher-${key}">
                <div class="col d-flex justify-content-start align-items-center"><span id="online-teacher-${key}" class="fas ${value.teacher.online ? 'fa-link' : 'fa-unlink'} mr-1"></span>${value.teacher.lastName}, ${value.teacher.firstName}</div>
                <div class="col d-flex justify-content-end"><a role="button" class="btn btn-sm btn-secondary" onclick="unfollow_teacher(${key})"><i class="fas fa-user-minus"></i></a></div>
            </div>
            `);
        }

        for (const [key, value] of Object.entries(user.students).sort((a, b) => (a.student.lastName + a.student.firstName).localeCompare(b.student.lastName + b.student.firstName))) {
            $('#students-list').append(`
            <div class="list-group-item list-group-item-action d-flex justify-content-between align-items-center" id="student-${key}">
                <div class="col d-flex justify-content-start align-items-center"><span id="online-student-${key}" class="fas ${value.student.online ? 'fa-link' : 'fa-unlink'} mr-1"></span>${value.student.lastName}, ${value.student.firstName}</div>
            </div>
            `);
        }

        ws = new WebSocket('ws://' + config.host + ':' + config.service_port + '/communication/?id=' + user.id, 'exploraa-ws');
        ws.onmessage = msg => {
            const c_msg = JSON.parse(msg.data);
            switch (c_msg.type) {
                case 'online':
                    if (c_msg.user in user.students) {
                        user.students[c_msg.user].online = c_msg.online;
                        $('#online-student-' + c_msg.user).removeClass('fa-link fa-unlink').addClass(c_msg.online ? 'fa-link' : 'fa-unlink');
                    }
                    if (c_msg.user in user.teachers) {
                        user.teachers[c_msg.user].online = c_msg.online;
                        $('#online-teacher-' + c_msg.user).removeClass('fa-link fa-unlink').addClass(c_msg.online ? 'fa-link' : 'fa-unlink');
                    }
                    break;
                case 'follower':
                    if (c_msg.added) {
                        fetch('http://' + config.host + ':' + config.service_port + '/students/' + c_msg.student, {
                            method: 'get',
                            headers: { 'Authorization': 'Basic ' + user.id }
                        }).then(response => {
                            if (response.ok) {
                                response.json().then(student => {
                                    user.students[student.id] = student;
                                    $('#students-list').append(`
                                    <div class="list-group-item list-group-item-action d-flex justify-content-between align-items-center" id="student-${student.id}">
                                        <div class="col d-flex justify-content-start align-items-center"><span id="online-student-${student.id}" class="fas ${student.online ? 'fa-link' : 'fa-unlink'} mr-1"></span>${student.lastName}, ${student.firstName}</div>
                                    </div>
                                    `);
                                });
                            } else
                                alert(response.statusText);
                        });
                    } else {
                        delete user.students[c_msg.student];
                        $('#student-' + c_msg.student).remove();
                    }
                    break;
                default:
                    console.log(msg);
                    break;
            }
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
        const teacher_id = this.getAttribute('teacher_id');
        fetch('http://' + config.host + ':' + config.service_port + '/follow/?student_id=' + user.id + '&teacher_id=' + teacher_id, {
            method: 'post',
            headers: { 'Authorization': 'Basic ' + user.id }
        }).then(response => {
            if (response.ok) {
                fetch('http://' + config.host + ':' + config.service_port + '/teachers/' + teacher_id, {
                    method: 'get',
                    headers: { 'Authorization': 'Basic ' + user.id }
                }).then(response => {
                    if (response.ok) {
                        response.json().then(teacher => {
                            user.teachers[teacher.id] = teacher;
                            $('#f-teachers-list').append(`
                            <div class="list-group-item list-group-item-action d-flex justify-content-between align-items-center" id="f-teacher-${teacher.id}">
                                <div class="col d-flex justify-content-start align-items-center"><span id="online-teacher-${teacher.id}" class="fas ${teacher.online ? 'fa-link' : 'fa-unlink'} mr-1"></span>${teacher.lastName}, ${teacher.firstName}</div>
                                <div class="col d-flex justify-content-end"><a role="button" class="btn btn-sm btn-secondary" onclick="unfollow_teacher(${teacher.id})"><i class="fas fa-user-minus"></i></a></div>
                            </div>
                            `);
                        });
                    } else
                        alert(response.statusText);
                });
            } else
                alert(response.statusText);
        });
    });
}

function unfollow_teacher(teacher_id) {
    fetch('http://' + config.host + ':' + config.service_port + '/unfollow/?student_id=' + user.id + '&teacher_id=' + teacher_id, {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            delete user.teachers[teacher_id];
            $('#f-teacher-' + teacher_id).remove();
        } else
            alert(response.statusText);
    });
}