export let user;
export let user_model;
$.get('user_model.json', function (data) { user_model = data; });
export const stimuli = []

export function set_user(new_user) {
    user = new_user;
}