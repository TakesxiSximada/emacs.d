import ControlPanel from './viewmodels'


export default class DefaultController {
    constructor (){
        this.controlPanel = new ControlPanel();
    }

    __call__ (){
        console.log('CALL ');
        console.log(this);
    }

    get url() {
        return this.controlPanel.url;
    }
}
