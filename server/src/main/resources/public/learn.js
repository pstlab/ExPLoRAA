import * as config from './config.js'
import * as context from './context.js'

export function init() {
    set_following_teachers();
    set_following_lessons();
    set_stimuli();
}

function set_stimuli() {
    const stimuli_list = $('#stimuli-list');
    const stimulus_row_template = $('#stimulus-row');
    for (const stimulus of context.stimuli.sort((a, b) => a[1].time > b[1].time))
        stimuli_list.append(create_stimulus_row(stimulus_row_template, stimulus));
}

export function new_stimulus(stimulus) {
    $('#stimuli-list').append(create_following_lesson_row($('#stimulus-row'), stimulus));
}

function set_following_teachers() {
    const teachers_list = $('#f-teachers-list');
    const teacher_row_template = $('#following-teacher-row');
    for (const [id, teacher] of Object.entries(context.user.teachers).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
        teachers_list.append(create_following_teacher_row(teacher_row_template, teacher));
}

function set_following_lessons() {
    const f_lessons_list = $('#f-lessons-list');
    const f_lesson_row_template = $('#following-lesson-row');
    for (const [id, lesson] of Object.entries(context.user.followingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
        f_lessons_list.append(create_following_lesson_row(f_lesson_row_template, lesson));
}

export function show_lessons_to_follow() {
    const lessons_list = $('#lessons-list');
    const lesson_row_template = $('#follow-new-lesson-row');
    lessons_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/lessons/' + context.user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + context.user.id }
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

export function follow_selected_lessons() {
    alert('not implemented yet..');
}

export function show_teachers_to_follow() {
    const teachers_list = $('#teachers-list');
    const teacher_row_template = $('#follow-new-teacher-row');
    teachers_list.empty();
    fetch('http://' + config.host + ':' + config.service_port + '/teachers/' + context.user.id, {
        method: 'get',
        headers: { 'Authorization': 'Basic ' + context.user.id }
    }).then(response => {
        if (response.ok) {
            response.json().then(data => {
                data.sort((a, b) => (a.lastName + a.firstName).localeCompare(b.lastName + b.firstName)).forEach(teacher => teachers_list.append(create_follow_new_teacher_row(teacher_row_template, teacher)));
                $('#show-teachers-modal').modal('show');
            });
        } else
            alert(response.statusText);
    });
}

export function follow_selected_teachers() {
    $('#teachers-list').find('input:checked').each(function () {
        fetch('http://' + config.host + ':' + config.service_port + '/user/follow/?student_id=' + context.user.id + '&teacher_id=' + this.teacher_id, {
            method: 'post',
            headers: { 'Authorization': 'Basic ' + context.user.id }
        }).then(response => {
            if (response.ok) {
                fetch('http://' + config.host + ':' + config.service_port + '/teacher/' + this.teacher_id, {
                    method: 'get',
                    headers: { 'Authorization': 'Basic ' + context.user.id }
                }).then(response => {
                    if (response.ok) {
                        response.json().then(teacher => {
                            context.user.teachers[teacher.id] = teacher;
                            $('#f-teachers-list').append(create_following_teacher_row($('#following-teacher-row'), teacher));
                        });
                    } else
                        alert(response.statusText);
                });
            } else
                alert(response.statusText);
        });
    });
}

export function set_online(teacher_id, online) {
    context.user.teachers[teacher_id].online = online;
    $('#online-teacher-' + teacher_id).removeClass(config.online_icon + ' ' + config.offline_icon).addClass(online ? config.online_icon : config.offline_icon);
}

function unfollow_teacher(teacher_id) {
    fetch('http://' + config.host + ':' + config.service_port + '/user/unfollow/?student_id=' + context.user.id + '&teacher_id=' + teacher_id, {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id }
    }).then(response => {
        if (response.ok) {
            delete context.user.teachers[teacher_id];
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
    return row_content;
}

function create_follow_new_teacher_row(template, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += teacher.id;
    input.teacher_id = teacher.id;
    const label = row_content.querySelector('label');
    label.append(teacher.lastName + ', ' + teacher.firstName);
    label.htmlFor = input.id;
    return row_content;
}

function create_following_teacher_row(template, teacher) {
    const teacher_row = template[0].content.cloneNode(true);
    const row_content = teacher_row.querySelector('.list-group-item');
    row_content.id += teacher.id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += teacher.id;
    online_span.classList.add(teacher.online ? config.online_icon : config.offline_icon);
    divs[0].append(teacher.lastName + ', ' + teacher.firstName);
    divs[1].childNodes[0].onclick = function () { unfollow_teacher(teacher.id); };
    return row_content;
}

function create_following_lesson_row(template, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += lesson.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    for (const stimulus of lesson.stimuli)
        context.stimuli.push(stimulus);
    return row_content;
}