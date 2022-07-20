import m from 'mithril'


export default class ControlPanel {
    constructor(){
        this.url = m.prop('https://www.google.co.jp');
        this.email = m.prop('');
        this.password = m.prop('');
        this.domains = new Set([]);
        this.update_domains();

        this._secret = m.prop('');  // real password
    }

    update_url(url){
        this.url(url);
        this.update_domains();
    }

    update_domains(){
        var url = new URL(this.url());
        url.hostname.split('.').forEach((value, ii, array) => {
            this.domains.add(value);
        });
    }

    register_secret(word){
        this._secret(word);
    }
}
