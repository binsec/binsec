/**************************************************************************/
/*  This file is part of Binsec.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2017                                               */
/*    VERIMAG                                                             */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/**************************************************************************/

#include<iostream>

#if defined(TARGET_LINUX)
/*
 * Server implementation for Linux that use directly ZMQ
 */

#include "pin.H"
#include <zmq.hpp>
#include "server_callback.hpp"

zmq::context_t context (1);
zmq::socket_t socket_s (context, ZMQ_DEALER);
zmq::message_t cmd;
zmq::message_t data;

bool islistening = false;

using namespace std;

bool initialize_server(std::string port) {
    string addr="tcp://*:"+port;
    socket_s.bind(addr.c_str());
	islistening = true;
	return true;
}

void send_message(std::string cmd, std::string data, bool blocking) {
    int flags = blocking ? 0 : ZMQ_DONTWAIT;
    zmq::message_t rep_cmd (cmd.size());
    zmq::message_t rep_data (data.size());
    memcpy (rep_cmd.data(), cmd.data(), cmd.size());
    memcpy (rep_data.data (), data.data(), data.size());
    socket_s.send(rep_cmd, flags | ZMQ_SNDMORE);
    socket_s.send(rep_data, flags);
    cout << "Reply sent" << endl;
}

bool check_messages(CONTEXT* ctx, THREADID thread_id, bool blocking) {
	int flag = blocking ? 0 : ZMQ_DONTWAIT;
    if(islistening){
        int len = socket_s.recv(&cmd, flag);
        if(len > 0) {
	    int len2 = socket_s.recv(&data, flag);
	    if(len2 > 0) {
            return dispatch_messages(ctx, thread_id, string((char*)cmd.data(),cmd.size()), string((char*)data.data(),data.size()));
	    }
	    else
	        cout << "Command received but not data" << endl;
    }
    }
    return true;
}

void shutdown_server() {
    if(islistening) {
        cout << "Server terminate" << endl;
        socket_s.close();
        zmq_term(context);
    }
}

#endif
