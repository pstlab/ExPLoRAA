import * as config from './config.js'
import * as context from './context.js'
import * as learn from "./learn.js";
import * as teach from "./teach.js";
import * as create from "./create.js";

let ws;

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

export function login() {
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

export function logout() {
    localStorage.removeItem('email');
    localStorage.removeItem('password');
    location.reload();
}

export function signin() {
    const email = $('#signin-email').val();
    const password = $('#signin-password').val();
    const first_name = $('#signin-first-name').val();
    const last_name = $('#signin-last-name').val();
    const profile = {};
    context.user_model.interests.forEach(element => {
        profile[element.id] = false;
    });
    const form = new FormData();
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

export function delete_user() {
    fetch('http://' + config.host + ':' + config.service_port + '/user/' + context.user.id, {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + context.user.id }
    }).then(response => {
        if (response.ok) {
            logout();
        } else
            alert(response.statusText);
    });
}

export function update_user() {
    context.user.firstName = $('#profile-first-name').val();
    context.user.lastName = $('#profile-last-name').val();

    const profile = {};
    context.user_model.interests.forEach(element => {
        profile[element.id] = $('#user-interest-' + to_id(element.id)).prop('checked');
    });
    context.user.profile = JSON.stringify(profile);
    fetch('http://' + config.host + ':' + config.service_port + '/user/' + context.user.id, {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: JSON.stringify(context.user)
    }).then(response => {
        if (response.ok) {
            $('#account-menu').text(context.user.firstName);
            $('#profile-modal').modal('hide');
        } else
            alert(response.statusText);
    });
}

function setUser(usr) {
    context.set_user(usr);
    $('#body-guest').remove();
    $.get('body_user.html', function (data) {
        $('#exploraa-body').append(data);
        $('#account-menu').text(context.user.firstName);

        $('#profile-email').val(context.user.email);
        $('#profile-first-name').val(context.user.firstName);
        $('#profile-last-name').val(context.user.lastName);

        const profile_form = $('#profile-form');
        const user_interest = $('#user-interest-row');
        context.user_model.interests.forEach(element => {
            create_user_interest_row(profile_form, user_interest, element.id, element);
        });

        const profile = JSON.parse(context.user.profile);
        Object.entries(profile).forEach(([key, value]) => {
            $('#user-interest-' + to_id(key)).prop('checked', value);
        });

        $('#learn').load('learn.html', () => {
            learn.set_following_teachers();
            learn.set_following_lessons();
            learn.set_stimuli();
        });

        $('#teach').load('teach.html', () => {
            teach.set_teaching_lessons();
            teach.set_students();
        });

        $('#create').load('create.html', () => {
            create.set_models();
        });

        ws = new WebSocket('ws://' + config.host + ':' + config.service_port + '/communication/?id=' + context.user.id, 'exploraa-ws');
        ws.onmessage = msg => {
            const c_msg = JSON.parse(msg.data);
            switch (c_msg.type) {
                case 'online':
                    if (c_msg.user in context.user.students)
                        teach.set_online(c_msg.user, c_msg.online);
                    if (c_msg.user in context.user.teachers)
                        learn.set_online(c_msg.user, c_msg.online);
                    break;
                case 'follower':
                    if (c_msg.added) {
                        fetch('http://' + config.host + ':' + config.service_port + '/student/' + c_msg.student, {
                            method: 'get',
                            headers: { 'Authorization': 'Basic ' + context.user.id }
                        }).then(response => {
                            if (response.ok) {
                                response.json().then(student => { teach.new_student(student); });
                            } else
                                alert(response.statusText);
                        });
                    } else
                        teach.remove_student(c_msg.student);
                    break;
                case 'profile-update':
                    teach.student_profile_changed(c_msg.user, c_msg.profile);
                    break;
                default:
                    console.log(msg);
                    break;
            }
        };
    });
}

function create_user_interest_row(interests_list, template, id, interest) {
    const interest_row = template[0].content.cloneNode(true);
    const row_content = interest_row.querySelector('.form-check');
    const input = row_content.querySelector('input');
    input.id += to_id(id);
    const label = row_content.querySelector('label');
    label.htmlFor = input.id;
    label.append(interest.name);
    interests_list.append(interest_row);
}

function to_id(id) { return id.replace('%27', '').replace(':', '-'); }