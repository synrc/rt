-ifndef(STREAMS_HRL).
-define(STREAMS_HRL, true).

-define(LIMIT, 1 * 1000 * 1000).

-record(gen_server, {circa,state,file,time=1,app=1,len,msg,acc_len,size,acc,acc_pred,init=1,sign,parent}).

-endif.
