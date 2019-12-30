var video_controller = {id:null, queue: [], type: "local"};
var video_timer = null;
var video_timer_active = false;
var yt_player = null;

function start_video_interval() {
    if (!video_timer_active) {
	video_timer = setInterval(video_manage, 500);
	video_timer_active = true;
	console.dir(video_timer_active);
    }
}
function stop_video_interval() {
    if (video_timer_active) {
	clearInterval(video_timer);
	video_timer_active = false;
	console.dir(video_timer_active);
    }
}

function enqueue(items, video_id, type) {
    stop_video_interval();
    var old_id = video_controller.video_id; // HTML element with player attached, if any
    video_controller = {id: video_id, queue: items, type: type};
    if (type == "youtube") {
	if (old_id != null && old_id != video_id) {
	    yt_player.destroy();
	    yt_player = null;
	}
	if (yt_player == null) {
	    yt_player = new YT.Player(video_id, {
		//height: '290',
		//width: '1200',
		videoId: items[0].video_src,
		events: {
                    'onReady': video_play,
		    'onStateChange': yt_player_state_change
		}
	    });
	} else {
	    // this won't work if video_id is new, i.e. the existing yt_player is attached to a different HTML element
	    video_play();
	}
    } else {
	// local media
	video_play();
    }
}

//player.setSize(width:Number, height:Number):Object

function yt_player_state_change(event) {
    if (event.data == YT.PlayerState.PAUSED || event.data == YT.PlayerState.ENDED) {
	stop_video_interval();
    } else if (event.data == YT.PlayerState.PLAYING) {
	start_video_interval();
    }
}

function video_play() {
    //console.dir(video_controller);
    if (video_controller.queue.length > 0) {
	var item = video_controller.queue[0];
	if (video_controller.type == "youtube") {
	    if (yt_player.getPlaylist() != null && yt_player.getPlaylist()[0] == item.video_src) {
		// same video, so just seek to right spot
		yt_player.seekTo(item.start_time);
	    } else {
		yt_player.loadPlaylist(item.video_src, 0, item.start_time);
	    }
	} else {
	    el = document.getElementById(video_controller.id);
	    //TODO set src here
	    el.currentTime = item.start_time;
	    el.play();
	}
	start_video_interval();
    } else {
	video_stop();
    }
}

function onYouTubeIframeAPIReady() {
    // don't need to do anything
}

function video_stop() {
    stop_video_interval();
    if (video_controller.type == "youtube") {
	yt_player.stopVideo();
    } else {
	document.getElementById(video_controller.id).pause();
    }
    video_controller = {id:null, queue: [], type: ""};
}

function video_manage() {
    if (video_controller.queue.length > 0) {
	var item = video_controller.queue[0];
	var current_time;
	var current_src;
	if (video_controller.type == "youtube") {
	    current_time = yt_player.getCurrentTime();
	    current_src = yt_player.getPlaylist()[0];
	} else {
	    var el = document.getElementById(video_controller.id);
	    current_time = el.currentTime;
	    current_src = el.getAttribute('src');
	}
	if (current_src != item.video_src) {
	    // we are out of whack somehow
	    console.log("src mismatch");
	    video_stop();
        } else 
	if (current_time > (item.start_time+item.duration)) {
	    //console.log("finished");
            video_controller.queue.shift();
	    video_play(); // next item
        } else {
	    // current item still playing
        }
    } else {
	// no items
	console.log("nothing to play");
        video_stop();
    }
}
