import * as config from './config.js'
import * as context from './context.js'

let current_model;
let current_rule;

export function init() {
    create_topics()
    set_models();

    $('#new-rule-type').on('change', function (e) {
        switch (this.value) {
            case '1':
                $('#new-rule-url-div').addClass('d-none');
                break;
            case '2':
                $('#new-rule-url-div').removeClass('d-none');
                break;
            case '3':
                $('#new-rule-url-div').addClass('d-none');
                break;
            default:
                break;
        }
    });

    $('#new-precondition-type').on('change', function (e) {
        switch (this.value) {
            case '1':
                $('#suggested-preconditions-list-div').removeClass('d-none');
                $('#existing-preconditions-list-div').addClass('d-none');
                break;
            case '2':
                $('#suggested-preconditions-list-div').addClass('d-none');
                $('#existing-preconditions-list-div').removeClass('d-none');
                break;
            case '3':
                $('#suggested-preconditions-list-div').addClass('d-none');
                $('#existing-preconditions-list-div').addClass('d-none');
                break;
            default:
                break;
        }
    });
}

function create_topics() {
    const topics_list = $('#topics-list');
    const topic_row_template = $('#topic-row');
    context.user_model.interests.sort((a, b) => a.name.localeCompare(b.name)).forEach(interest => {
        create_topic_row(topics_list, topic_row_template, interest);
    });
}

function set_models() {
    const models_list = $('#models-list');
    const model_row_template = $('#model-row');
    for (const [id, model] of Object.entries(context.user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
        create_model_row(models_list, model_row_template, model);
}

export function new_model(model) {
    create_model_row($('#models-list'), $('#model-row'), model);
}

export function create_new_model() {
    const form = new FormData();
    form.append('name', $('#new-template-name').val());
    form.append('teacher_id', context.user.id);
    fetch('http://' + config.host + ':' + config.service_port + '/model', {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            $('#new-template-name').val('');
            response.json().then(model => {
                context.user.models[model.id] = model;
                create_model_row($('#models-list'), $('#model-row'), model);
            });
        } else
            alert(response.statusText);
    });
}

function delete_model(model_id) {
    fetch('http://' + config.host + ':' + config.service_port + '/model/' + model_id, {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + context.user.id }
    }).then(response => {
        if (response.ok) {
            delete context.user.models[model_id];
            $('#model-' + model_id).remove();
            if (current_model.id == model_id)
                $('#model').removeClass('active');
        } else
            alert(response.statusText);
    });
}

export function create_new_rule() {
    const form = new FormData();
    form.append('model_id', current_model.id);
    form.append('name', $('#new-rule-name').val());
    switch ($('#new-rule-type').val()) {
        case '1':
            form.append('type', 'text');
            break;
        case '2':
            form.append('type', 'web');
            break;
        case '3':
            form.append('type', 'wiki');
            break;
        default:
            break;
    }
    fetch('http://' + config.host + ':' + config.service_port + '/rule', {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            $('#new-rule-name').val('');
            $('#new-rule-type').val('');
            response.json().then(rule => {
                current_model.rules[rule.id] = rule;
                create_rule_row($('#rules-list'), $('#rule-row'), rule);
                create_existing_precondition_row($('#existing-preconditions-list'), $('#existing-precondition-row'), rule);
            });
        } else
            alert(response.statusText);
    });
}

export function create_new_preconditions() {
    switch ($('#new-precondition-type').val()) {
        case '1':
            $('#suggested-preconditions-list').find('input:checked').each(function () {
                const form = new FormData();
                form.append('model_id', current_model.id);
                form.append('effect_id', current_rule.id);
                form.append('type', current_rule.type);
                form.append('name', this.suggestion_id);

                fetch('http://' + config.host + ':' + config.service_port + '/rule', {
                    method: 'post',
                    headers: { 'Authorization': 'Basic ' + context.user.id },
                    body: form
                }).then(response => {
                    if (response.ok) {
                        response.json().then(rule => {
                            current_rule.preconditions.push(rule);
                            create_precondition_row($('#preconditions-list'), $('#precondition-row'), rule);
                            create_rule_row($('#rules-list'), $('#rule-row'), rule);
                            create_existing_precondition_row($('#existing-preconditions-list'), $('#existing-precondition-row'), rule);
                        });
                    } else
                        alert(response.statusText);
                });
            });
            break;
        case '2':
            $('#existing-preconditions-list').find('input:checked').each(function () {
                const form = new FormData();
                form.append('condition_id', precondition_id);
                form.append('effect_id', current_rule.id);

                fetch('http://' + config.host + ':' + config.service_port + '/precondition', {
                    method: 'post',
                    headers: { 'Authorization': 'Basic ' + context.user.id },
                    body: form
                }).then(response => {
                    if (response.ok) {
                        response.json().then(rule => {
                            current_model.rules[rule.id] = rule;
                            create_precondition_row($('#preconditions-list'), $('#precondition-row'), rule);
                        });
                    } else
                        alert(response.statusText);
                });
            });
            break;
        case '3':
            break;
        default:
            break;
    }
}

function delete_precondition(precondition_id) {
    const form = new FormData();
    form.append('condition_id', precondition_id);
    form.append('effect_id', current_rule.id);
    fetch('http://' + config.host + ':' + config.service_port + '/precondition', {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            delete current_rule.preconditions[precondition_id];
            $('#precondition-' + precondition_id).remove();
        } else
            alert(response.statusText);
    });
}

function create_model_row(models_list, template, model) {
    const model_row = template[0].content.cloneNode(true);
    const row_content = model_row.querySelector('.list-group-item');
    row_content.id += model.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(model.name);
    divs[1].childNodes[0].onclick = function () { delete_model(model.id); };
    models_list.append(model_row);

    $('#model-' + model.id).on('show.bs.tab', function (event) {
        current_model = model;

        // we set the rules..
        const sorted_rules = Object.entries(model.rules).sort((a, b) => a[1].name.localeCompare(b[1].name));
        const rules_list = $('#rules-list');
        rules_list.empty();
        const rule_row_template = $('#rule-row');
        for (const [id, rule] of sorted_rules)
            create_rule_row(rules_list, rule_row_template, rule);

        // we set the possible preconditions..
        const existing_preconditions_list = $('#existing-preconditions-list');
        existing_preconditions_list.empty();
        const existing_precondition_row_template = $('#existing-precondition-row');
        for (const [id, rule] of sorted_rules)
            create_existing_precondition_row(existing_preconditions_list, existing_precondition_row_template, rule);

        $('#rule').removeClass('active');
    });
}

function create_rule_row(rules_list, template, rule) {
    const rule_row = template[0].content.cloneNode(true);
    const row_content = rule_row.querySelector('.list-group-item');
    row_content.id += rule.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(rule.name);
    rules_list.append(rule_row);

    $('#rule-' + rule.id).on('show.bs.tab', function (event) {
        current_rule = rule;

        $('#rule-id').val(rule.id);
        $('#rule-name').val(rule.name);

        $('#rule-type').on('change', function (e) {
            switch (this.value) {
                case '1':
                    $('#rule-url-div').addClass('d-none');
                    break;
                case '2':
                    $('#rule-url-div').removeClass('d-none');
                    break;
                case '3':
                    $('#rule-url-div').removeClass('d-none');
                    break;
                default:
                    break;
            }
        });

        switch (rule.type) {
            case 'text':
                $('#rule-type').val(1);
                $('#rule-url-div').addClass('d-none');
                break;
            case 'web':
                $('#rule-type').val(2);
                $('#rule-url-div').removeClass('d-none');
                $('#rule-url').val(rule.url);
                break;
            case 'wiki':
                $('#rule-type').val(3);
                $('#rule-url-div').removeClass('d-none');
                $('#rule-url').val(rule.url);
                break;
            default:
                break;
        }

        context.user_model.interests.forEach(interest => {
            $('#topic-' + context.to_id(interest.id)).prop('checked', rule.topics.includes(interest.id));
        });

        $('#rule-length').val(rule.length);

        const suggested_preconditions_list = $('#suggested-preconditions-list');
        suggested_preconditions_list.empty();
        const suggestion_row_template = $('#suggested-precondition-row');
        rule.suggestions.forEach(suggestion => {
            create_suggested_precondition_row(suggested_preconditions_list, suggestion_row_template, suggestion);
        });

        const preconditions_list = $('#preconditions-list');
        preconditions_list.empty();
        const precondition_row_template = $('#precondition-row');
        rule.preconditions.forEach(precondition_id => {
            create_precondition_row(preconditions_list, precondition_row_template, current_model.rules[precondition_id]);
        });
    });
}

function create_topic_row(topics_list, template, topic) {
    const interest_row = template[0].content.cloneNode(true);
    const row_content = interest_row.querySelector('.form-check');
    const input = row_content.querySelector('input');
    input.id += context.to_id(topic.id);
    const label = row_content.querySelector('label');
    label.htmlFor = input.id;
    label.append(topic.name);
    topics_list.append(interest_row);
}

function create_suggested_precondition_row(suggestions_list, template, suggestion) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += suggestion;
    input.suggestion_id = suggestion;
    const label = row_content.querySelector('label');
    label.append(suggestion);
    label.htmlFor = input.id;
    suggestions_list.append(suggestion_row);
}

function create_existing_precondition_row(suggestions_list, template, rule) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += rule.id;
    input.rule_id += rule.id;
    const label = row_content.querySelector('label');
    label.append(rule.name);
    label.htmlFor = input.id;
    suggestions_list.append(suggestion_row);
}

function create_precondition_row(preconditions_list, template, precondition) {
    const precondition_row = template[0].content.cloneNode(true);
    const row_content = precondition_row.querySelector('.list-group-item');
    row_content.id += precondition.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(precondition.name);
    divs[1].childNodes[0].onclick = function () { delete_precondition(precondition.id); };
    preconditions_list.append(precondition_row);
}