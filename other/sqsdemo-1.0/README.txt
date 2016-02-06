erlcloud

prerequisite:
    meck, https://github.com/eproxus/meck.git
    jsx, https://github.com/talentdeficit/jsx.git
    lhttpc, https://github.com/talko/lhttpc


run

$ ./start.bat


code:
    sqs.erl ----- send and retreive and delete message
    dynamodb.erl  -------- put items and get items and delete items


imports:
    sqs:
        1.
        you need configuration access key and secret access key in  two ways:
            a). call configure
                erlcloud_sqs:configure(AccessKeyID, SecretAccessKey, Host).
                but the configuration just work in same process.
            b). use config parameter when call receive_message. spec for receive_message. same for other function.
                -spec receive_message/2 :: (string(), [sqs_msg_attribute_name()] | all | aws_config()) -> proplist().
                receive_message(QueueName, Config)

        2.
        when receive message from sqs, the message is pushed in message in flight. some times it's will repush in message queue. when you retrieve message from sqs, you need to delete the message by receipt_handle propperty in message.
        for example:
                % 3 in parameters is stand for the limits retrieve message's count one time.
                [{messages, Messages}] = erlcloud_sqs:receive_message(QueueName, [], 3),
                lists:foreach(fun (Message) ->
                    % business logic
                    Body = proplists:get_value(body, Message),
                    Handle = proplists:get_value(receipt_handle, Message),
                    io:format("queue : ~s message : ~s ~n", [QueueName, Body]),
                    % delete message
                    ok = erlcloud_sqs:delete_message(QueueName, Handle)
                end, Messages),

    dynamodb:
        don't support boolean type.

