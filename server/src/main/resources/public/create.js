export class Create {

    constructor() {
        this.current_model = undefined;
        this.current_rule = undefined;
    }

    set_models(models) {
        const models_list = $('#models-list');
        const model_row_template = $('#model-row');
        for (const [id, model] of Object.entries(models).sort((a, b) => a[1].name.localeCompare(b[1].name)))
            create_model_row(models_list, model_row_template, id, model);
    }

    new_model(model) {
        create_model_row($('#models-list'), $('#model-row'), model.id, model);
    }

    new_model() {
        const form = new FormData();
        form.append('name', $('#new-template-name').val());
        form.append('teacher_id', user.id);
        fetch('http://' + config.host + ':' + config.service_port + '/model', {
            method: 'post',
            headers: { 'Authorization': 'Basic ' + user.id },
            body: form
        }).then(response => {
            if (response.ok) {
                $('#new-template-name').val('');
                response.json().then(model => {
                    user.models[model.id] = model;
                    create_model_row($('#models-list'), $('#model-row'), model.id, model);
                });
            } else
                alert(response.statusText);
        });
    }

    delete_model(model_id) {
        fetch('http://' + config.host + ':' + config.service_port + '/model/' + model_id, {
            method: 'delete',
            headers: { 'Authorization': 'Basic ' + user.id }
        }).then(response => {
            if (response.ok) {
                delete user.models[model_id];
                $('#model-' + model_id).remove();
                if (current_model == model_id)
                    $('#model').removeClass('active');
            } else
                alert(response.statusText);
        });
    }
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
            default:
                break;
        }
        $('#rule-length').val(rule.length);
    });
}

function create_suggestion_row(template, id, suggestion) {
    const suggestion_row = template[0].content.cloneNode(true);
    const row_content = suggestion_row.querySelector('.list-group-item');
    row_content.id += id;
    const divs = row_content.querySelectorAll('div');
    divs[0].append(suggestion.name);
    return suggestion_row;
}