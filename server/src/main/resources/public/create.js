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
        topics_list.append(create_topic_row(topic_row_template, interest));
    });
}

function set_models() {
    const models_list = $('#models-list');
    const model_row_template = $('#model-row');
    for (const [id, model] of Object.entries(context.user.models).sort((a, b) => a[1].name.localeCompare(b[1].name))) {
        const model_row = create_model_row(model_row_template, model);
        models_list.append(model_row);
        set_model_row_show_event(model_row, model);
    }
}

export function new_model(model) {
    const model_row = create_model_row($('#model-row'), model);
    $('#models-list').append(model_row);
    set_model_row_show_event(model_row, model);
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
                const model_row = create_model_row($('#model-row'), model);
                $('#models-list').append(model_row);
                set_model_row_show_event(model_row, model);
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
    const c_model = current_model;
    const rule_name = $('#new-rule-name').val();
    const form = new FormData();
    form.append('model_id', c_model.id);
    form.append('name', rule_name);
    switch ($('#new-rule-type').val()) {
        case '1':
            form.append('type', 'text');
            break;
        case '2':
            form.append('type', 'web');
            form.append('url', $('#new-rule-url').val());
            break;
        case '3':
            form.append('type', 'wiki');
            break;
        default:
            break;
    }
    const rule_row = create_rule_row($('#rule-row'), { name: rule_name });
    $('#rules-list').append(rule_row);
    fetch('http://' + config.host + ':' + config.service_port + '/rule', {
        method: 'post',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            $('#new-rule-name').val('');
            $('#new-rule-type').val('');
            $('#new-rule-url').val('');
            response.json().then(rule => {
                c_model.rules[rule.id] = rule;
                refine_rule_row(rule_row, rule);
                $('#existing-preconditions-list').append(create_existing_precondition_row($('#existing-precondition-row'), rule));
            });
        } else
            alert(response.statusText);
    });
}

export function create_new_preconditions() {
    const c_model = current_model;
    const c_rule = current_rule;
    switch ($('#new-precondition-type').val()) {
        case '1':
            $('#suggested-preconditions-list').find('input:checked').each(function () {
                const form = new FormData();
                form.append('model_id', c_model.id);
                form.append('effect_id', c_rule.id);
                form.append('type', c_rule.type);
                form.append('name', this.suggestion_id);

                const rule_row = create_rule_row($('#rule-row'), { name: this.suggestion_id });
                $('#rules-list').append(rule_row);
                fetch('http://' + config.host + ':' + config.service_port + '/rule', {
                    method: 'post',
                    headers: { 'Authorization': 'Basic ' + context.user.id },
                    body: form
                }).then(response => {
                    if (response.ok) {
                        response.json().then(rule => {
                            c_model.rules[rule.id] = rule;
                            c_rule.preconditions.push(rule.id);
                            refine_rule_row(rule_row, rule);
                            if (c_rule == current_rule)
                                $('#preconditions-list').append(create_precondition_row($('#precondition-row'), rule));
                            $('#existing-preconditions-list').append(create_existing_precondition_row($('#existing-precondition-row'), rule));
                        });
                    } else
                        alert(response.statusText);
                });
            });
            break;
        case '2':
            $('#existing-preconditions-list').find('input:checked').each(function () {
                const form = new FormData();
                form.append('condition_id', this.rule_id);
                form.append('effect_id', c_rule.id);

                fetch('http://' + config.host + ':' + config.service_port + '/precondition', {
                    method: 'post',
                    headers: { 'Authorization': 'Basic ' + context.user.id },
                    body: form
                }).then(response => {
                    if (response.ok) {
                        c_rule.preconditions.push(this.rule_id);
                        if (c_rule == current_rule)
                            $('#preconditions-list').append(create_precondition_row($('#precondition-row'), c_model.rules[this.rule_id]));
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
    const c_rule = current_rule;
    const form = new FormData();
    form.append('condition_id', precondition_id);
    form.append('effect_id', c_rule.id);
    fetch('http://' + config.host + ':' + config.service_port + '/precondition', {
        method: 'delete',
        headers: { 'Authorization': 'Basic ' + context.user.id },
        body: form
    }).then(response => {
        if (response.ok) {
            delete c_rule.preconditions[precondition_id];
            $('#precondition-' + precondition_id).remove();
        } else
            alert(response.statusText);
    });
}

function create_model_row(template, model) {
    const model_row = template[0].content.cloneNode(true);
    const row_content = model_row.querySelector('.list-group-item');
    row_content.id += model.id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(model.name);
    divs[1].childNodes[0].onclick = function () { delete_model(model.id); };
    return model_row;
}

function set_model_row_show_event(model_row, model) {
    $('#model-' + model.id).on('show.bs.tab', function (event) {
        current_model = model;

        // we set the rules..
        const sorted_rules = Object.entries(model.rules).sort((a, b) => a[1].name.localeCompare(b[1].name));
        const rules_list = $('#rules-list');
        rules_list.empty();
        const rule_row_template = $('#rule-row');
        for (const [id, rule] of sorted_rules) {
            const rule_row = create_rule_row(rule_row_template, rule);
            rules_list.append(rule_row);
            refine_rule_row(rule_row, rule);
        }

        // we set the possible preconditions..
        const existing_preconditions_list = $('#existing-preconditions-list');
        existing_preconditions_list.empty();
        const existing_precondition_row_template = $('#existing-precondition-row');
        for (const [id, rule] of sorted_rules)
            existing_preconditions_list.append(create_existing_precondition_row(existing_precondition_row_template, rule));

        $('#rule').removeClass('active');
    });
}

function create_rule_row(template, rule) {
    const rule_row = template[0].content.cloneNode(true);
    const row_content = rule_row.querySelector('.list-group-item');
    const a = row_content.querySelector('a');
    a.append(rule.name);
    return row_content;
}

function refine_rule_row(rule_row, rule) {
    rule_row.id += rule.id;
    rule_row.querySelector('.spinner-border').remove();

    if (rule.type == 'web' || rule.type == 'wiki')
        rule_row.querySelector('a').href = rule.url;

    $('#rule-' + rule.id).removeClass('disabled');
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

        if (rule.top_down)
            $('#approach').val(1);
        else
            $('#approach').val(2);

        context.user_model.interests.forEach(interest => {
            $('#topic-' + context.to_id(interest.id)).prop('checked', rule.topics.includes(interest.id));
        });

        $('#rule-length').val(rule.length);

        const suggested_preconditions_list = $('#suggested-preconditions-list');
        suggested_preconditions_list.empty();
        const suggestion_row_template = $('#suggested-precondition-row');
        rule.suggestions.forEach(suggestion => {
            suggested_preconditions_list.append(create_suggested_precondition_row(suggestion_row_template, suggestion));
        });

        const preconditions_list = $('#preconditions-list');
        preconditions_list.empty();
        const precondition_row_template = $('#precondition-row');
        rule.preconditions.forEach(precondition_id => {
            preconditions_list.append(create_precondition_row(precondition_row_template, current_model.rules[precondition_id]));
        });
    });
}

function create_topic_row(template, topic) {
    const interest_row = template[0].content.cloneNode(true);
    const row_content = interest_row.querySelector('.form-check');
    const input = row_content.querySelector('input');
    input.id += context.to_id(topic.id);
    const label = row_content.querySelector('label');
    label.htmlFor = input.id;
    label.append(topic.name);
    return interest_row;
}

function create_suggested_precondition_row(template, suggestion) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += suggestion;
    input.suggestion_id = suggestion;
    const label = row_content.querySelector('label');
    label.append(suggestion);
    label.htmlFor = input.id;
    return suggestion_row;
}

function create_existing_precondition_row(template, rule) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    const input = row_content.querySelector('input');
    input.id += rule.id;
    input.rule_id = rule.id;
    const label = row_content.querySelector('label');
    label.append(rule.name);
    label.htmlFor = input.id;
    return suggestion_row;
}

function create_precondition_row(template, precondition) {
    const precondition_row = template[0].content.cloneNode(true);
    const row_content = precondition_row.querySelector('.list-group-item');
    row_content.id += precondition.id;
    const divs = row_content.querySelectorAll('div');
    const a = divs[0].querySelector('a')
    if (precondition.type == 'web' || precondition.type == 'wiki')
        a.href = precondition.url;
    a.append(precondition.name);
    divs[1].childNodes[0].onclick = function () { delete_precondition(precondition.id); };
    return precondition_row;
}