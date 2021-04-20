export class Teach {

    constructor(user_model) {
        this.user_model = user_model;
        this.current_student = undefined;
    }

    set_teaching_lessons(lessons) {
        const t_lessons_list = $('#t-lessons-list');
        const t_lesson_row_template = $('#teaching-lesson-row');
        for (const [id, lesson] of Object.entries(lessons).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_teaching_lesson_row(t_lessons_list, t_lesson_row_template, id, lesson);
    }

    new_teaching_lesson(lesson) {
        create_teaching_lesson_row($('#t-lessons-list'), $('#teaching-lesson-row'), lesson.id, lesson);
    }

    set_students(students) {
        const students_list = $('#students-list');
        const student_row_template = $('#student-row');
        for (const [id, student] of Object.entries(students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            create_student_row(students_list, student_row_template, id, student);
    }

    new_student(student) {
        create_student_row($('#students-list'), $('#student-row'), student.id, student);
    }

    remove_student(student_id) {
        $('#student-' + student_id).remove();
        if (current_student == student_id)
            $('#student').removeClass('active');
    }

    student_profile_changed(student_id, json_profile) {
        if (current_student == student_id) {
            const c_profile = JSON.parse(json_profile);
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
    }

    show_create_lesson() {
        const models_list = $('#new-lesson-model');
        models_list.empty();
        for (const [id, model] of Object.entries(user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            models_list.append($('<option>', { value: id, text: model.name }));

        const students_list = $('#new-lesson-students-list');
        const student_row_template = $('#new-lesson-student-row');
        students_list.empty();
        for (const [id, student] of Object.entries(user.students).sort((a, b) => (a[1].lastName + a[1].firstName).localeCompare(b[1].lastName + b[1].firstName)))
            create_new_lesson_student_row(students_list, student_row_template, id, student);

        $('#new-lesson-modal').modal('show');
    }
}

function new_lesson() {

}

function create_teaching_lesson_row(lessons_list, template, id, lesson) {
    const lesson_row = template[0].content.cloneNode(true);
    const row_content = lesson_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(lesson.name);
    lessons_list.append(lesson_row);
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