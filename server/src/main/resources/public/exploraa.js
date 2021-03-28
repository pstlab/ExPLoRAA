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
    fetch('http://' + config.host + ':' + config.service_port + '/user', {
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
    fetch('http://' + config.host + ':' + config.service_port + '/user/' + user.id, {
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
    const profile = {};
    profile.antro = $('#profile-antro').prop('checked');
    profile.art = $('#profile-art').prop('checked');
    profile.biog = $('#profile-biog').prop('checked');
    profile.biol = $('#profile-biol').prop('checked');
    profile.phil = $('#profile-phil').prop('checked');
    profile.geo = $('#profile-geo').prop('checked');
    profile.math = $('#profile-math').prop('checked');
    profile.sci = $('#profile-sci').prop('checked');
    profile.soc = $('#profile-soc').prop('checked');
    profile.hist = $('#profile-hist').prop('checked');
    profile.tech = $('#profile-tech').prop('checked');
    user.profile = JSON.stringify(profile);
    fetch('http://' + config.host + ':' + config.service_port + '/user/' + user.id, {
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
        const profile = JSON.parse(user.profile);
        $('#profile-antro').prop('checked', profile.antro);
        $('#profile-art').prop('checked', profile.art);
        $('#profile-biog').prop('checked', profile.biog);
        $('#profile-biol').prop('checked', profile.biol);
        $('#profile-phil').prop('checked', profile.phil);
        $('#profile-geo').prop('checked', profile.geo);
        $('#profile-math').prop('checked', profile.math);
        $('#profile-sci').prop('checked', profile.sci);
        $('#profile-soc').prop('checked', profile.soc);
        $('#profile-hist').prop('checked', profile.hist);
        $('#profile-tech').prop('checked', profile.tech);

        // we set the stimuli..
        user.stimuli = [];

        // we set the teachers..
        const teachers_list = $('#f-teachers-list');
        const teacher_row_template = $('#teacher-row');
        for (const [id, teacher] of Object.entries(user.teachers).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            teachers_list.append(create_teacher_row(teacher_row_template, id, teacher));

        // we set the following lessons..
        const f_lessons_list = $('#f-lessons-list');
        const f_lesson_row_template = $('#f-lesson-row');
        for (const [id, lesson] of Object.entries(user.followingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name))) {
            f_lessons_list.append(create_following_lesson_row(f_lesson_row_template, id, lesson));
            for (const stimulus of lesson.stimuli)
                user.stimuli.push(stimulus);
        }

        // we set the stimuli..
        const stimuli_list = $('#stimuli-list');
        const stimulus_row_template = $('#stimulus-row');
        for (const stimulus of user.stimuli.sort((a, b) => a[1].time > b[1].time))
            stimuli_list.append(create_stimulus_row(stimulus_row_template, stimulus));

        // we set the teaching lessons..
        const t_lessons_list = $('#t-lessons-list');
        const t_lesson_row_template = $('#t-lesson-row');
        for (const [id, lesson] of Object.entries(user.teachingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            t_lessons_list.append(create_teaching_lesson_row(t_lesson_row_template, id, lesson));

        // we set the students..
        const students_list = $('#students-list');
        const student_row_template = $('#student-row');
        for (const [id, student] of Object.entries(user.students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            students_list.append(create_student_row(student_row_template, id, student));

        // we set the lesson templates..
        const lesson_models_list = $('#lesson-models-list');
        const lesson_model_row_template = $('#lesson-model-row');
        for (const [id, model] of Object.entries(user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            lesson_models_list.append(create_lesson_model_row(lesson_model_row_template, id, model));

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
                        fetch('http://' + config.host + ':' + config.service_port + '/student/' + c_msg.student, {
                            method: 'get',
                            headers: { 'Authorization': 'Basic ' + user.id }
                        }).then(response => {
                            if (response.ok) {
                                response.json().then(student => {
                                    user.students[student.id] = student;
                                    students_list.append(create_student_row(student_row_template, student.id, student));
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

function show_lessons() {
    const lessons_list = $('#lessons-list');
    const lesson_row_template = $('#follow-lesson-row');
    lessons_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/lessons/' + user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.forEach(lesson => lessons_list.append(create_follow_teacher_row(lesson_row_template, lesson.id, lesson)));
                $('#show-lessons-modal').modal('show');
            });
        } else
            alert(response.statusText);
    });
}

function show_teachers() {
    const teachers_list = $('#teachers-list');
    const teacher_row_template = $('#follow-teacher-row');
    teachers_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/teachers/' + user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.forEach(teacher => teachers_list.append(create_follow_teacher_row(teacher_row_template, teacher.id, teacher)));
                $('#show-teachers-modal').modal('show');
            });
        } else
            alert(response.statusText);
    });
}

function follow_teachers() {
    $('#teachers-list').find('input:checked').each(function () {
        const teacher_id = this.getAttribute('teacher_id');
        fetch('http://' + config.host + ':' + config.service_port + '/user/follow/?student_id=' + user.id + '&teacher_id=' + teacher_id, {
            method: 'post',
            headers: { 'Authorization': 'Basic ' + user.id }
        }).then(response => {
            if (response.ok) {
                fetch('http://' + config.host + ':' + config.service_port + '/teacher/' + teacher_id, {
                    method: 'get',
                    headers: { 'Authorization': 'Basic ' + user.id }
                }).then(response => {
                    if (response.ok) {
                        response.json().then(teacher => {
                            user.teachers[teacher.id] = teacher;
                            $('#f-teachers-list').append(create_teacher_row($('#teacher-row'), teacher.id, teacher));
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
    fetch('http://' + config.host + ':' + config.service_port + '/user/unfollow/?student_id=' + user.id + '&teacher_id=' + teacher_id, {
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

function create_stimulus_row(template, stimulus) {
    const stimulus_row = template[0].content.cloneNode(true);
    const row_content = stimulus_row.querySelector('.list-group-item');
    row_content.id += stimulus.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(row_content.id);
    return stimulus_row;
}

function create_follow_lesson_row(template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.childNodes[0].id += id;
    row_content.childNodes[0].setAttribute('lesson_id', id);
    row_content.childNodes[1].htmlFor += id;
    row_content.childNodes[1].append(lesson.name);
    return lesson_row;
}

function create_following_lesson_row(template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    return lesson_row;
}

function create_teaching_lesson_row(template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    return lesson_row;
}

function create_follow_teacher_row(template, id, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    row_content.childNodes[0].id += id;
    row_content.childNodes[0].setAttribute('teacher_id', id);
    row_content.childNodes[1].htmlFor += id;
    row_content.childNodes[1].append(teacher.lastName + ', ' + teacher.firstName);
    return teacher_row;
}

function create_teacher_row(template, id, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += id;
    if (teacher.online)
        online_span.className = 'fas fa-link mr-1';
    else
        online_span.className = 'fas fa-unlink mr-1';
    divs[0].append(teacher.lastName + ', ' + teacher.firstName);
    divs[1].childNodes[0].onclick = function () { unfollow_teacher(id); };
    return teacher_row;
}

function create_student_row(template, id, student) {
    const student_row = template[0].content.cloneNode(true);
    const row_content = student_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += id;
    if (student.online)
        online_span.className = 'fas fa-link mr-1';
    else
        online_span.className = 'fas fa-unlink mr-1';
    divs[0].append(student.lastName + ', ' + student.firstName);
    return student_row;
}

function create_lesson_model_row(template, id, lesson_model) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson_model.name);
    return lesson_row;
}

function create_stimulus_template(template, id, stimulus) {
    const c_template = template[0].content.cloneNode(true);
    const stimulus_id = rule_row.querySelector('.stimulus-id');
    stimulus_id.append(id);
    const stimulus_name = rule_row.querySelector('.stimulus-name');
    stimulus_name.append(stimulus.name);
    return c_template;
}

function create_rule_row(template, id, rule) {
    const rule_row = template[0].content.cloneNode(true);
    const row_content = rule_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(rule.name);
    return rule_row;
}

function create_suggestion_row(template, id, suggestion) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(suggestion.name);
    return suggestion_row;
}