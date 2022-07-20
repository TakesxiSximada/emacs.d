import m from 'mithril'

import clipboard from 'clipboard-js'

import ControlPanel from './viewmodels'
import getPassword from './password'
import views from 'msx-loader!./views.msx'

class Password {
    constructor(panel) {
        this.panel = panel;
    }
    calc_and_copy(num, domain){
        var panel = this.panel;
        return function (data){
            var pswd = getPassword(domain, panel.email(), panel.password(), num);
            clipboard.copy(pswd);
        };
    }
}

function DefaultController(){
    var controlPanel = new ControlPanel();
    var password = new Password(controlPanel);
    return function (){
        return {
            controlPanel: controlPanel,
            password: password,
            update_url: (data) => controlPanel.update_url(data),
        }
    }
}

function DefaultView(){
    return function (ctrl){
        return [
            views.header(ctrl),
            views.section(ctrl),
            views.footer(ctrl),
        ];
    }
}

m.mount(document.body, {
    view: DefaultView(),
    controller: DefaultController(),
});
