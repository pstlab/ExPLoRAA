export class Learn {

    constructor() {
        this.stimuli = []
    }

    set_stimuli() {
        const stimuli_list = $('#stimuli-list');
        const stimulus_row_template = $('#stimulus-row');
        for (const stimulus of stimuli.sort((a, b) => a[1].time > b[1].time))
            create_stimulus_row(stimuli_list, stimulus_row_template, stimulus);
    }

    new_stimulus(stimulus) {
        create_following_lesson_row($('#stimuli-list'), $('#stimulus-row'), stimulus.id, stimulus);
    }

    set_following_teachers(teachers) {
        const teachers_list = $('#f-teachers-list');
        const teacher_row_template = $('#following-teacher-row');
        for (const [id, teacher] of Object.entries(teachers).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            create_following_teacher_row(teachers_list, teacher_row_template, id, teacher);
    }

    new_following_teacher(teacher) {
        create_following_teacher_row($('#f-teachers-list'), $('#following-teacher-row'), teacher.id, teacher);
    }

    set_following_lessons(lessons) {
        const f_lessons_list = $('#f-lessons-list');
        const f_lesson_row_template = $('#following-lesson-row');
        for (const [id, lesson] of Object.entries(lessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_following_lesson_row(f_lessons_list, f_lesson_row_template, id, lesson);
    }

    new_following_lesson(lesson) {
        create_following_lesson_row($('#f-lessons-list'), $('#following-lesson-row'), lesson.id, lesson);
    }

    show_lessons() {
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

    show_teachers() {
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

    follow_teachers() {
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

function create_stimulus_row(stimuli_list, template, stimulus) {
    const stimulus_row = template[0].content.cloneNode(true);
    const row_content = stimulus_row.querySelector('.list-group-item');
    row_content.id += stimulus.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(row_content.id);
    stimuli_list.append(stimulus_row);
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