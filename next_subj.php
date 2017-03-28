<?php
	/*
	next_subj.json looks like this:
	{"init:0"}
	init can be any number. if it is 0, then the first list number
	will be 1. Each subject has a unique ID in the database. This ID
	is associated with a unique list number that is incremented when
	a participant accesses the experiment. For example, if a subject with
	ID = 4 accesses the experiment, then next_subj.json will be updated to
	this: {"init":0,"4":1}. The next participant might have ID = 1, then
	next_subj.json will update to {"init":0,"4":1,"1":2}. When the participant
	logs back in after perhaps stopping midway through they will always receive the same list number.
	For example, subject 4 will receive list 1, and subject 1 will receive list 1.
	*/
	
	require_once '../../../wp-config.php';
	global $current_user;
	get_currentuserinfo();
	$user_ID=$current_user->ID;

	//check if user_ID exists
	$subj_arr = json_decode(file_get_contents('next_subj.json'), true);
	if(array_key_exists((string)$user_ID, $subj_arr)){
		//if it exists get the list
		$subj_num = $subj_arr[$user_ID];
		echo $subj_num;
	}else{
		//if it does not exist bind it to a list
		$new_subj_num = max($subj_arr) + 1;
		$subj_arr[$user_ID] = $new_subj_num;
		file_put_contents('next_subj.json',json_encode($subj_arr));
		echo $new_subj_num;
	}

?>
