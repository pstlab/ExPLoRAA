import * as config from './config.js'
import * as context from './context.js'

export let current_model;
export let current_rule;

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
}

function create_topics() {
    const topics_list = $('#topics-list');
    const topic_row_template = $('#topic-row');
    context.user_model.interests.sort((a, b) => a.name.localeCompare(b.name)).forEach(element => {
        create_topic_row(topics_list, topic_row_template, element.id, element);
    });
}

function set_models() {
    const models_list = $('#models-list');
    const model_row_template = $('#model-row');
    for (const [id, model] of Object.entries(context.user.models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
        create_model_row(models_list, model_row_template, id, model);
}

export function new_model(model) {
    create_model_row($('#models-list'), $('#model-row'), model.id, model);
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
                create_model_row($('#models-list'), $('#model-row'), model.id, model);
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
            if (current_model == model_id)
                $('#model').removeClass('active');
        } else
            alert(response.statusText);
    });
}

export function create_new_rule() {
    const form = new FormData();
    form.append('model_id', current_model);
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
                context.user.models[current_model].rules[rule.id] = rule;
                create_rule_row($('#rules-list'), $('#rule-row'), rule.id, rule);
            });
        } else
            alert(response.statusText);
    });
}

export function create_new_precondition() {
    alert('not implemented yet..');
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

        // we set the rules..
        const rules_list = $('#rules-list');
        rules_list.empty();
        const rule_row_template = $('#rule-row');
        for (const [id, rule] of Object.entries(model.rules).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_rule_row(rules_list, rule_row_template, id, rule);

        $('#rule').removeClass('active');
    });
}

function create_rule_row(rules_list, template, id, rule) {
    const rule_row = template[0].content.cloneNode(true);
    const row_content = rule_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(rule.name);
    rules_list.append(rule_row);

    $('#rule-' + id).on('show.bs.tab', function (event) {
        current_rule = id;

        $('#rule-id').val(id);
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

        const suggestions_list = $('#preconditions-list');
        suggestions_list.empty();
        const suggestion_row_template = $('#suggested-precondition-row');
        rule.suggestions.forEach(suggestion => {
            create_suggested_precondition_row(suggestions_list, suggestion_row_template, suggestion);
        });
    });
}

function create_topic_row(topics_list, template, id, topic) {
    const interest_row = template[0].content.cloneNode(true);
    const row_content = interest_row.querySelector('.form-check');
    const input = row_content.querySelector('input');
    input.id += context.to_id(id);
    const label = row_content.querySelector('label');
    label.htmlFor = input.id;
    label.append(topic.name);
    topics_list.append(interest_row);
}

function create_suggested_precondition_row(suggestions_list, template, suggestion) {
    const stimulus_row = template[0].content.cloneNode(true);
    const row_content = stimulus_row.querySelector('.list-group-item');
    row_content.id += suggestion;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(suggestion);
    suggestions_list.append(stimulus_row);
}

function create_suggested_rule_row(suggestions_list, template, suggestion) {
    const stimulus_row = template[0].content.cloneNode(true);
    const row_content = stimulus_row.querySelector('.list-group-item');
    row_content.id += suggestion;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(suggestion);
    suggestions_list.append(stimulus_row);
}