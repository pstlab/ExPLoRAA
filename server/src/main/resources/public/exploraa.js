const config = {
    'host': '192.168.1.101',
    'service_port': 7000,
    'websocket_port': 8884
};
const online_icon = 'bi-check-circle';
const offline_icon = 'bi-circle';
let user;
let ws;
const stimuli = [];
let current_student;
let current_model;

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
    const profile = {
        antro: false,
        art: false,
        biog: false,
        biol: false,
        phil: false,
        geo: false,
        math: false,
        sci: false,
        soc: false,
        hist: false,
        tech: false
    };
    const form = new FormData();
    form.append('email', email);
    form.append('password', password);
    form.append('first_name', first_name);
    form.append('last_name', last_name);
    form.append('email', email);
    form.append('password', password);
    form.append('first_name', first_name);
    form.append('last_name', last_name);
    form.append('profile', JSON.stringify(profile));
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
        stimuli.length = 0;

        // we set the teachers..
        const teachers_list = $('#f-teachers-list');
        const teacher_row_template = $('#following-teacher-row');
        for (const [id, teacher] of Object.entries(user.teachers).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            create_following_teacher_row(teachers_list, teacher_row_template, id, teacher);

        // we set the following lessons..
        const f_lessons_list = $('#f-lessons-list');
        const f_lesson_row_template = $('#following-lesson-row');
        for (const [id, lesson] of Object.entries(user.followingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_following_lesson_row(f_lessons_list, f_lesson_row_template, id, lesson);

        // we set the stimuli..
        const stimuli_list = $('#stimuli-list');
        const stimulus_row_template = $('#stimulus-row');
        for (const stimulus of stimuli.sort((a, b) => a[1].time > b[1].time))
            create_stimulus_row(stimuli_list, stimulus_row_template, stimulus);

        // we set the teaching lessons..
        const t_lessons_list = $('#t-lessons-list');
        const t_lesson_row_template = $('#teaching-lesson-row');
        for (const [id, lesson] of Object.entries(user.teachingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_teaching_lesson_row(t_lessons_list, t_lesson_row_template, id, lesson);

        // we set the students..
        const students_list = $('#students-list');
        const student_row_template = $('#student-row');
        for (const [id, student] of Object.entries(user.students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            create_student_row(students_list, student_row_template, id, student);

        // we set the models..
        const models_list = $('#models-list');
        const model_row_template = $('#model-row');
        for (const [id, model] of Object.entries(user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_model_row(models_list, model_row_template, id, model);

        ws = new WebSocket('ws://' + config.host + ':' + config.service_port + '/communication/?id=' + user.id, 'exploraa-ws');
        ws.onmessage = msg => {
            const c_msg = JSON.parse(msg.data);
            switch (c_msg.type) {
                case 'online':
                    if (c_msg.user in user.students) {
                        user.students[c_msg.user].online = c_msg.online;
                        $('#online-student-' + c_msg.user).removeClass(online_icon + ' ' + offline_icon).addClass(c_msg.online ? online_icon : offline_icon);
                    }
                    if (c_msg.user in user.teachers) {
                        user.teachers[c_msg.user].online = c_msg.online;
                        $('#online-teacher-' + c_msg.user).removeClass(online_icon + ' ' + offline_icon).addClass(c_msg.online ? online_icon : offline_icon);
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
                                    create_student_row(students_list, student_row_template, student.id, student);
                                });
                            } else
                                alert(response.statusText);
                        });
                    } else {
                        delete user.students[c_msg.student];
                        $('#student-' + c_msg.student).remove();
                        if (current_student == c_msg.student)
                            $('#student').removeClass('active');
                    }
                    break;
                case 'profile-update':
                    user.students[c_msg.user].profile = c_msg.profile;
                    if (current_student == c_msg.user) {
                        const c_profile = JSON.parse(c_msg.profile);
                        $('#student-profile-antro').prop('checked', c_profile.antro);
                        $('#student-profile-art').prop('checked', c_profile.art);
                        $('#student-profile-biog').prop('checked', c_profile.biog);
                        $('#student-profile-biol').prop('checked', c_profile.biol);
                        $('#student-profile-phil').prop('checked', c_profile.phil);
                        $('#student-profile-geo').prop('checked', c_profile.geo);
                        $('#student-profile-math').prop('checked', c_profile.math);
                        $('#student-profile-sci').prop('checked', c_profile.sci);
                        $('#student-profile-soc').prop('checked', c_profile.soc);
                        $('#student-profile-hist').prop('checked', c_profile.hist);
                        $('#student-profile-tech').prop('checked', c_profile.tech);
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
    const lesson_row_template = $('#follow-new-lesson-row');
    lessons_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/lessons/' + user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.sort((a, b) => a[1].name.localeCompare(b[1].name)).forEach(lesson => create_follow_new_lesson_row(lessons_list, lesson_row_template, lesson.id, lesson));
                $('#show-lessons-modal').modal('show');
            });
        } else
            alert(response.statusText);
    });
}

function show_teachers() {
    const teachers_list = $('#teachers-list');
    const teacher_row_template = $('#follow-new-teacher-row');
    teachers_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/teachers/' + user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)).forEach(teacher => create_follow_new_teacher_row(teachers_list, teacher_row_template, teacher.id, teacher));
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
                            create_following_teacher_row($('#f-teachers-list'), $('#following-teacher-row'), teacher.id, teacher);
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

function new_model() {
    const form = new FormData();
    form.append('name', $('#new-template-name').val());
    form.append('teacher_id', user.id);
    fetch('http://' + config.host + ':' + config.service_port + '/model', {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            $('#new-template-name').val('');
            response.json().then(model => {
                user.models[model.id] = model;
                create_model_row($('#models-list'), $('#model-row'), model.id, model);
            });
        } else
            alert(response.statusText);
    });
}

function delete_model(model_id) {
    fetch('http://' + config.host + ':' + config.service_port + '/model/' + model_id, {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            delete user.models[model_id];
            $('#model-' + model_id).remove();
            if (current_model == model_id)
                $('#model').removeClass('active');
        } else
            alert(response.statusText);
    });
}

function create_stimulus_row(stimuli_list, template, stimulus) {
    const stimulus_row = template[0].content.cloneNode(true);
    const row_content = stimulus_row.querySelector('.list-group-item');
    row_content.id += stimulus.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(row_content.id);
    stimuli_list.append(stimulus_row);
}

function create_follow_new_lesson_row(lessons_list, template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.childNodes[0].id += id;
    row_content.childNodes[0].setAttribute('lesson_id', id);
    row_content.childNodes[1].htmlFor += id;
    row_content.childNodes[1].append(lesson.name);
    lessons_list.append(lesson_row);
}

function create_following_lesson_row(lessons_list, template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    lessons_list.append(lesson_row);
    for (const stimulus of lesson.stimuli)
        stimuli.push(stimulus);
}

function create_teaching_lesson_row(lessons_list, template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    lessons_list.append(lesson_row);
}

function create_follow_new_teacher_row(teachers_list, template, id, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    row_content.childNodes[0].id += id;
    row_content.childNodes[0].setAttribute('teacher_id', id);
    row_content.childNodes[1].htmlFor += id;
    row_content.childNodes[1].append(teacher.lastName + ', ' + teacher.firstName);
    teachers_list.append(teacher_row);
}

function create_following_teacher_row(teachers_list, template, id, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += id;
    online_span.classList.add(teacher.online ? online_icon : offline_icon);
    divs[0].append(teacher.lastName + ', ' + teacher.firstName);
    divs[1].childNodes[0].onclick = function () { unfollow_teacher(id); };
    teachers_list.append(teacher_row);
}

function create_student_row(students_list, template, id, student) {
    const student_row = template[0].content.cloneNode(true);
    const row_content = student_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += id;
    online_span.classList.add(student.online ? online_icon : offline_icon);
    divs[0].append(student.lastName + ', ' + student.firstName);
    students_list.append(student_row);

    $('#student-' + id).on('show.bs.tab', function (event) {
        current_student = id;
        $('#student-email').val(student.email);
        $('#student-first-name').val(student.firstName);
        $('#student-last-name').val(student.lastName);
        const profile = JSON.parse(student.profile);
        $('#student-profile-antro').prop('checked', profile.antro);
        $('#student-profile-art').prop('checked', profile.art);
        $('#student-profile-biog').prop('checked', profile.biog);
        $('#student-profile-biol').prop('checked', profile.biol);
        $('#student-profile-phil').prop('checked', profile.phil);
        $('#student-profile-geo').prop('checked', profile.geo);
        $('#student-profile-math').prop('checked', profile.math);
        $('#student-profile-sci').prop('checked', profile.sci);
        $('#student-profile-soc').prop('checked', profile.soc);
        $('#student-profile-hist').prop('checked', profile.hist);
        $('#student-profile-tech').prop('checked', profile.tech);
    });
}

function create_model_row(models_list, template, id, model) {
    const model_row = template[0].content.cloneNode(true);
    const row_content = model_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(model.name);
    divs[1].childNodes[0].onclick = function () { delete_model(id); };
    models_list.append(model_row);

    $('#model-' + id).on('show.bs.tab', function (event) {
        current_model = id;
    });
}

function create_stimulus_model(template, id, stimulus) {
    const model = template[0].content.cloneNode(true);
    const stimulus_id = rule_row.querySelector('.stimulus-id');
    stimulus_id.append(id);
    const stimulus_name = rule_row.querySelector('.stimulus-name');
    stimulus_name.append(stimulus.name);
    return model;
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