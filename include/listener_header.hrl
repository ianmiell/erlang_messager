%TODO: sort out configuration
-define(LISTENER_PORT, 12345).

-define(NUM_ACCEPTOR_THREADS, 20000).
-define(NUM_CLIENTS, 20000).
-define(PER_THREAD_LOGGING_MOD, 4999).
-define(NEW_SOCKET_START_WAIT,2).
-define(MESSAGE_SEND_DELAY,5000).

-define(CONN_TABLE,connection_table).

-define(LOG_SERVER,log_server).
-define(LOG_LEVEL,1000).

-define(ROUTER_SERVER_NAME, "localhost").
-define(ROUTER_SERVER_PORT, 9909).
-define(ROUTER_CLIENT_RETRY_TIME, 1000).
-define(ROUTER_CLIENT_TIMEOUT, 30000).
-define(RECEIVE_BUFFER_LENGTH, 4096).
