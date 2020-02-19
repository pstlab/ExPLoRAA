let user;
let mqtt_client;
let config = {
    'host': '150.146.65.22',
    'service_port': 8181,
    'websocket_port': 8080
};

$(window).on('load', function () {
    let email = localStorage.getItem('email');
    let password = localStorage.getItem('password');
    if (email && password) {
        let form = new FormData();
        form.append('email', email);
        form.append('password', password);
        fetch('https://' + config.host + ':' + config.service_port + '/login', {
            method: 'post',
            body: form
        }).then(response => {
            if (response.ok) {
                response.json().then(data => { setUser(data); });
            } else {
                localStorage.removeItem('email');
                localStorage.removeItem('password');
                $.get('login_form.html', function (data) { $('#nav-bar').append(data); });
                $.get('signin_form.html', function (data) { $('#body').append(data); });
            }
        });
    } else {
        $.get('login_form.html', function (data) { $('#nav-bar').append(data); });
        $.get('signin_form.html', function (data) { $('#body').append(data); });
    }
});

function login() {
    let email = $('#login-email').val();
    let password = $('#login-password').val();
    let form = new FormData();
    form.append('email', email);
    form.append('password', password);
    fetch('https://' + config.host + ':' + config.service_port + '/login', {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $('#navbar-content').remove();
            $('#signin-form').remove();
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            response.json().then(data => { setUser(data); });
        } else
            alert(response.statusText);
    });
}

function logout() {
    mqtt_client.disconnect();
    localStorage.removeItem('email');
    localStorage.removeItem('password');
    location.reload(false);
}

function signin() {
    let email = $('#signin-email').val();
    let password = $('#signin-password').val();
    let first_name = $('#signin-first-name').val();
    let last_name = $('#signin-last-name').val();
    let form = new FormData();
    form.append('email', email);
    form.append('password', password);
    form.append('first_name', first_name);
    form.append('last_name', last_name);
    fetch('https://' + config.host + ':' + config.service_port + '/users', {
        method: 'post',
        body: form
    }).then(response => {
        if (response.ok) {
            $('#navbar-content').remove();
            $('#signin-form').remove();
            localStorage.setItem('email', email);
            localStorage.setItem('password', password);
            location.reload(false);
        } else
            alert(response.statusText);
    });
}

function deleteUser() {
    fetch('https://' + config.host + ':' + config.service_port + '/users/' + user.id, {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + user.id }
    }).then(response => {
        if (response.ok) {
            mqtt_client.disconnect();
            localStorage.removeItem('email');
            localStorage.removeItem('password');
            location.reload(false);
        } else
            alert(response.statusText);
    });
}

function setUser(usr) {
    user = usr;
    $.get('nav_bar.html', function (data) {
        $('#nav-bar').append(data);
        if (user.firstName) {
            $('#account-menu').text(user.firstName);
        }

        $('#profile').on('show.bs.modal', function (event) {
            $('#e-mail').val(user.email);
            $('#first-name').val(user.firstName);
            $('#last-name').val(user.lastName);
            $('#save-profile').click(function () {
                user.firstName = $('#first-name').val();
                user.lastName = $('#last-name').val();
                fetch('https://' + config.host + ':' + config.service_port + '/users/' + user.id, {
                    method: 'patch',
                    headers: { 'Authorization': 'Basic ' + user.id },
                    body: JSON.stringify(user)
                }).then(response => {
                    if (response.ok) {
                        if (user.firstName) {
                            $('#account-menu').text(user.firstName);
                        }
                        $('#profile').modal('hide');
                    } else
                        alert(response.statusText);
                });
            });
        })

        $.get('body.html', function (data) {
            $('#body').append(data);

            let stimuli = [];
            if (user.following)
                for (const [key, value] of Object.entries(user.following)) {
                    $('#f-lessons-list').append('<a class=\'list-group-item list-group-item-action\' data-toggle=\'list\' href=\'#l' + key + '\'role=\'tab\'>' + value.lesson.name + '</a>');
                    $('#f-lesson').append('<div class=\'tab-pane fade\' id=\'l' + key + '\' role=\'tabpanel\'>' + value.lesson.name + '</div>');
                }

            let students = [];
            if (user.teaching)
                for (const [key, value] of Object.entries(user.teaching)) {
                    $('#t-lessons-list').append('<a class=\'list-group-item list-group-item-action\' data-toggle=\'list\' href=\'#l' + key + '\'role=\'tab\'>' + value.lesson.name + '</a>');
                    $('#t-lesson').append('<div class=\'tab-pane fade\' id=\'l' + key + '\' role=\'tabpanel\'>' + value.lesson.name + '</div>');
                }
        });
    });

    // Create a client instance
    mqtt_client = new Paho.MQTT.Client(config.host, config.websocket_port, 'user-' + user.id);

    // set callback handlers
    // called when the client loses its connection
    mqtt_client.onConnectionLost = function (responseObject) {
        if (responseObject.errorCode !== 0) {
            console.log('onConnectionLost:' + responseObject.errorMessage);
        }
        user.online = false;
    };
    // called when a message arrives
    mqtt_client.onMessageArrived = function (message) {
        console.log('onMessageArrived:' + message.payloadString);
    };

    // connect the client
    var options = {
        useSSL: true,
        onSuccess: function () { // called when the client connects
            // Once a connection has been made, make a subscription and send a message.
            console.log('onConnect');
            user.online = true;
            mqtt_client.subscribe('ExPLoRAA/' + user.id + '/#', { qos: 2 });
            message = new Paho.MQTT.Message('Hello');
            message.destinationName = 'ExPLoRAA/' + user.id;
            mqtt_client.send(message);
        },
        onFailure: function () { // called when the client fails to connect
            console.log('onFailure');
        }
    };
    mqtt_client.connect(options);
}