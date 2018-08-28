export OUR_EMACS="/srv/ge"
# alias ew='emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init -nw --load .emacs.d/init.el'
# alias ewq='emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init -nw'
# alias ge='emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init --load .emacs.d/init.el'
# alias geq='emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init'
export GE_CONFIG=~/.config/ge.txt

function ge-set () {
    env_name="$1"
    env_path="$2"

    init_el=`echo "$env_path/.emacs.d/init.el" | sed -e 's/\/\+/\//g'`

    if [ "$env_name" = "" -o "$env_path" = "" ]
    then
	echo "Need parameter: ge use \$(NAME) \$(PATH)."
	return
    fi

    if [ ! -f "$init_el" ]
    then
	echo "Environment file not found: $init_el."
	return
    fi
    export GE_CURRENT=$env_name
    echo "$env_name:$env_path" >> ~/.config/ge.txt
    cat ~/.config/ge.txt | sort -u > ~/.config/ge.txt.tmp
    cp ~/.config/ge.txt.tmp ~/.config/ge.txt
    cat ~/.config/ge.txt
}


function ge-current () {
    env_name=$1
    if [ ! "$env_name" = "" ]
    then

	if [ ! `grep "^$env_name:" $GE_CONFIG` ]
	then
	    echo "Environ not found in config files: no $env_name in $GE_CONFIG"
	    return
	fi
	export GE_CURRENT=$env_name
    fi
    echo $GE_CURRENT
}


function ge-list () {
    cat $GE_CURRENT
}


function ge-start () {
    env_name="$1"
    option="$2"
    if [ "$env_name" = "" ]
    then
	env_name=$GE_CURRENT
    fi

    env_path=`grep "^$env_name:" $GE_CONFIG | cut -d ':' -f 2`
    init_el=`echo "$env_path/.emacs.d/init.el" | sed -e 's/\/\+/\//g'`
    if [ ! $env_path ]
    then
	echo "Environ not found in config files: no $env_name in $GE_CONFIG: using current directory settings."
	init_el=`echo "$PWD/$env_name/.emacs.d/init.el" | sed -e 's/\/\+/\//g'`
    fi

    if [ ! -f $init_el ]
    then
	echo "Environ .emacs.d/init.el not found: $init_el"
	return
    fi

    echo $init_el
    nohup emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init --load $init_el $option &
}

function ge-plain () {
    nohup emacs -q --no-site-file --no-splash --no-x-resources -f package-initialize --debug-init $1 &
}

function ge () {
    subcmd="$1"

    case "$subcmd" in
	"current" ) ge-current $2;;
	"set" ) ge-set $2 $3;;
	"g" ) ge-start $2;;
	"c" ) ge-start $2 "-nw";;
	"gq" ) ge-plain "";;
	"cq" ) ge-plain "-nw";;
	"" ) ge-start $2;;
	"*" ) echo "Invalid sub command: $subcmd: current, set, g, c, gq, cq or blank ";;
    esac
}
