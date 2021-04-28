import * as config from './config.js'
import * as context from './context.js'

let current_student = undefined;

export function init() {
    create_student_interests();
    set_teaching_lessons();
    set_students();
}

function set_teaching_lessons() {
    const t_lessons_list = $('#t-lessons-list');
    const t_lesson_row_template = $('#teaching-lesson-row');
    for (const [id, lesson] of Object.entries(context.user.teachingLessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
        t_lessons_list.append(create_teaching_lesson_row(t_lesson_row_template, lesson));
}

function create_student_interests() {
    const student_interests_list = $('#student-interests-list');
    const user_interest = $('#student-interest-row');
    context.user_model.interests.sort((a, b) => a.name.localeCompare(b.name)).forEach(element => {
        student_interests_list.append(create_student_interest_row(user_interest, element));
    });
}

function set_students() {
    const students_list = $('#students-list');
    const student_row_template = $('#student-row');
    for (const [id, student] of Object.entries(context.user.students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName))) {
        const student_row = create_student_row(student_row_template, student);
        students_list.append(student_row);
        refine_student_row(student_row, student);
    }
}

export function new_student(student) {
    context.user.students[student.id] = student;
    const student_row = create_student_row($('#student-row'), student);
    $('#students-list').append(student_row);
    refine_student_row(student_row, student);
}

export function remove_student(student_id) {
    delete context.user.students[student_id];
    $('#student-' + student_id).remove();
    if (current_student && current_student.id == student_id)
        $('#student').removeClass('active');
}

export function set_online(student_id, online) {
    context.user.students[student_id].online = online;
    $('#online-student-' + student_id).removeClass(config.online_icon + ' ' + config.offline_icon).addClass(online ? config.online_icon : config.offline_icon);
}

export function student_profile_changed(student_id, json_profile) {
    context.user.students[student_id].profile = json_profile;
    if (current_student && current_student.id == student_id) {
        const profile = JSON.parse(json_profile);
        Object.entries(profile).forEach(([key, value]) => {
            $('#student-interest-' + context.to_id(key)).prop('checked', value);
        });
    }
}

export function show_create_new_lesson() {
    const models_list = $('#new-lesson-model');
    models_list.empty();
    for (const [id, model] of Object.entries(context.user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
        models_list.append($('<option>', { value: id, text: model.name }));

    const students_list = $('#new-lesson-students-list');
    const student_row_template = $('#new-lesson-student-row');
    students_list.empty();
    for (const [id, student] of Object.entries(context.user.students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
        students_list.append(create_new_lesson_student_row(student_row_template, student));

    $('#new-lesson-modal').modal('show');
}

export function create_new_lesson() {
    const form = new FormData();
    form.append('name', $('#new-lesson-name').val());
    form.append('model_id', $('#new-lesson-model').val());
    form.append('teacher_id', context.user.id);
    const students_ids = [];

    $('#new-lesson-students-list').find('input:checked').each(function () {
        students_ids.push(parseInt(this.student_id));
    });

    const goals_ids = [];
    form.append('students_ids', students_ids);
    form.append('goals_ids', goals_ids);

    fetch('http://' + config.host + ':' + config.service_port + '/lesson', {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            $('#new-template-name').val('');
            response.json().then(lesson => {
                const teaching_lesson_row = create_teaching_lesson_row($('#teaching-lesson-row'), lesson);
                $('#t-lessons-list').append(teaching_lesson_row);
            });
        } else
            alert(response.statusText);
    });
}

function create_teaching_lesson_row(template, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += lesson.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    return row_content;
}

function create_new_lesson_student_row(template, student) {
    const student_row = template[0].content.cloneNode(true);
    const row_content = student_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += student.id;
    input.teacher_id = student.id;
    const label = row_content.querySelector('label');
    label.append(student.lastName + ', ' + student.firstName);
    label.htmlFor = input.id;
    return row_content;
}

function create_student_row(template, student) {
    const student_row = template[0].content.cloneNode(true);
    const row_content = student_row.querySelector('.list-group-item');
    row_content.id += student.id;
    const divs = row_content.querySelectorAll('div');
    var online_span = divs[0].childNodes[0];
    online_span.id += student.id;
    online_span.classList.add(student.online ? config.online_icon : config.offline_icon);
    divs[0].append(student.lastName + ', ' + student.firstName);
    return row_content;
}

function refine_student_row(student_row, student) {
    $('#student-' + student.id).on('show.bs.tab', function (event) {
        current_student = student;
        $('#student-email').val(student.email);
        $('#student-first-name').val(student.firstName);
        $('#student-last-name').val(student.lastName);

        const profile = JSON.parse(student.profile);
        Object.entries(profile).forEach(([key, value]) => {
            $('#student-interest-' + context.to_id(key)).prop('checked', value);
        });
    });
}

function create_student_interest_row(template, interest) {
    const interest_row = template[0].content.cloneNode(true);
    const row_content = interest_row.querySelector('.form-check');
    const input = row_content.querySelector('input');
    input.id += context.to_id(interest.id);
    const label = row_content.querySelector('label');
    label.htmlFor = input.id;
    label.append(interest.name);
    return row_content;
}