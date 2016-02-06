-record(multipart_list, {part_number, etag}).

-record(upload_session, {sha256, filepath, pid, in_date, expire_date, upload_id}).
-record(aws_multipart_upload_session, {unique_id, etag, part_number,  size}).
-record(awzsetting, {awshost, bucketname, accesskeyid, secretaccesskey, region}).


-record(block_chunk, {filename, start, stop, binary}).
-record(block_cache, {key, filename, blocks}).

-define(TIMEOUT, 600000).
