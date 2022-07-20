function bin2char(num, solt){
    solt = solt % 5;
    while(!((48 <= num && num <= 57) ||
	        (65 <= num && num <= 90) ||
	        (97 <= num && num <= 122))){
	    num = (48 + num + solt) % 123;
    }
    return String.fromCharCode(num)
}



export default function getPassword(word0, word1, solt, length){
    let before_num = 3;
    let ret_word = '';
    let  line = word1 + word0 + solt;
    let _nums = sha256.dec(line);  // orz
    for(let ii = 0; ii < length; ii++){
	    let _num = _nums[ii];
	    let _chr = bin2char(_num, before_num);
	    before_num = _num;
	    ret_word = ret_word + _chr;
    }
    return ret_word;
}
