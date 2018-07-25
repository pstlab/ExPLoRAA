/* global config */
var target = "http://" + config.host + ":" + config.service_port + "/ExPLoRAA/resources/";

$(document).ready(function () {
    console.log("retrieving users..");
    $.ajax({
        url: target + "users"
    }).then(function (users) {
        console.log("found " + users.length + " users..");
        document.getElementById("users_count").textContent = users.length;

        var tbl = document.getElementById("users_table");
        var tbdy = document.createElement("tbody");
        for (var i = 0; i < users.length; i++) {
            var tr = document.createElement("tr");

            var td_email = document.createElement("td");
            td_email.appendChild(document.createTextNode(users[i].email));
            tr.appendChild(td_email);

            var td_first_name = document.createElement("td");
            td_first_name.appendChild(document.createTextNode(users[i].first_name));
            tr.appendChild(td_first_name);

            var td_last_name = document.createElement("td");
            td_last_name.appendChild(document.createTextNode(users[i].last_name));
            tr.appendChild(td_last_name);

            var td_actions = document.createElement("td");
            var actions_group = document.createElement("div");
            actions_group.className = "btn-group";
            var btn_edit = document.createElement("button");
            btn_edit.type = "button";
            btn_edit.className = "btn btn-sm";
            btn_edit.disabled = true;
            var btn_edit_icon = document.createElement("i");
            btn_edit_icon.className = "fas fa-edit";
            btn_edit.append(btn_edit_icon);
            actions_group.append(btn_edit);
            var btn_del = document.createElement("button");
            btn_del.type = "button";
            btn_del.className = "btn btn-sm";
            var btn_del_icon = document.createElement("i");
            btn_del_icon.className = "fas fa-trash";
            btn_del.append(btn_del_icon);
            var user_id = users[i].id;
            btn_del.onclick = function () {
                $.ajax({
                    url: target + "user/" + user_id,
                    type: "DELETE"
                }).then(function () {
                    tbdy.removeChild(tr);
                    document.getElementById("users_count").textContent = users.length;
                });
            };
            actions_group.append(btn_del);
            td_actions.append(actions_group);
            tr.appendChild(td_actions);

            tbdy.appendChild(tr);
        }
        tbl.appendChild(tbdy);
    }, function (sender, message, details) {
        console.log("failed at loading users..");
    });

    console.log("retrieving lessons..");
    $.ajax({
        url: target + "lessons"
    }).then(function (lessons) {
        console.log("found " + lessons.length + " lessons..");
        document.getElementById("lessons_count").textContent = lessons.length;

        var tbl = document.getElementById("lessons_table");
        var tbdy = document.createElement("tbody");
        for (var i = 0; i < lessons.length; i++) {
            var tr = document.createElement("tr");

            var td_name = document.createElement("td");
            td_name.appendChild(document.createTextNode(lessons[i].name));
            tr.appendChild(td_name);

            var td_state = document.createElement("td");
            td_state.appendChild(document.createTextNode(lessons[i].state));
            tr.appendChild(td_state);

            var td_time = document.createElement("td");
            td_time.appendChild(document.createTextNode(lessons[i].time));
            tr.appendChild(td_time);

            var td_actions = document.createElement("td");
            var actions_group = document.createElement("div");
            actions_group.className = "btn-group";
            var btn_edit = document.createElement("button");
            btn_edit.type = "button";
            btn_edit.className = "btn btn-sm";
            btn_edit.disabled = true;
            var btn_edit_icon = document.createElement("i");
            btn_edit_icon.className = "fas fa-edit";
            btn_edit.append(btn_edit_icon);
            actions_group.append(btn_edit);
            var btn_del = document.createElement("button");
            btn_del.type = "button";
            btn_del.className = "btn btn-sm";
            btn_del.lesson_id = lessons[i].id;
            var btn_del_icon = document.createElement("i");
            btn_del_icon.className = "fas fa-trash";
            btn_del.append(btn_del_icon);
            var lesson_id = lessons[i].id;
            btn_del.onclick = function () {
                $.ajax({
                    url: target + "lesson/" + lesson_id,
                    type: "DELETE"
                }).then(function () {
                    tbdy.removeChild(tr);
                    document.getElementById("lessons_count").textContent = lessons.length;
                });
            };
            actions_group.append(btn_del);
            td_actions.append(actions_group);
            tr.appendChild(td_actions);

            tbdy.appendChild(tr);
        }
        tbl.appendChild(tbdy);
    }, function (sender, message, details) {
        console.log("failed at loading lessons..");
    });
});