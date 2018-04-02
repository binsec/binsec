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
#include "server_callback.hpp"

#if defined(TARGET_WINDOWS)

namespace WINDOWS {
#include<Windows.h>
}

#define ZMQ_DONWAIT 1
#define ZMQ_DEALER 5
#define ZMQ_SNDMORE 2

typedef void* (*zmq_ctx_new_t)();
typedef void* (*zmq_socket_t)(void*, int);
typedef int(*zmq_bind_t)(void*, const char*);
typedef int(*zmq_connect_t)(void*, const char*);
typedef int(*zmq_send_t)(void*, void*, size_t, int);
typedef int(*zmq_recv_t)(void*, void*, size_t, int);
typedef int(*zmq_close_t)(void*);
typedef int(*zmq_ctx_term_t)(void*);

zmq_ctx_new_t zmq_ctx_new = nullptr;
zmq_socket_t zmq_socket = nullptr;
zmq_bind_t zmq_bind = nullptr;
zmq_connect_t zmq_connect = nullptr;
zmq_send_t zmq_send = nullptr;
zmq_recv_t zmq_recv = nullptr;
zmq_close_t zmq_close = nullptr;
zmq_ctx_term_t zmq_ctx_term = nullptr;

WINDOWS::HMODULE libzmq = nullptr;

void* ctx;
void* socket;
char cmd[10];
char data[10000000];

bool islistening = false;


bool initialize_server(std::string port) {
    libzmq = WINDOWS::LoadLibrary("libzmq-v120-mt-4_0_4.dll");
    if (libzmq == NULL) {
        std::cout << "Fail to initialize libzmq-v120-mt-4_0_4.dll" << std::endl;
        return false;
    }

    zmq_ctx_new = (zmq_ctx_new_t)GetProcAddress(libzmq, "zmq_ctx_new");
    zmq_socket = (zmq_socket_t)GetProcAddress(libzmq, "zmq_socket");
    zmq_bind = (zmq_bind_t)GetProcAddress(libzmq, "zmq_bind");
    zmq_connect = (zmq_connect_t)GetProcAddress(libzmq, "zmq_connect");
    zmq_send = (zmq_send_t)GetProcAddress(libzmq, "zmq_send");
    zmq_recv = (zmq_recv_t)GetProcAddress(libzmq, "zmq_recv");
    zmq_close = (zmq_close_t)GetProcAddress(libzmq, "zmq_close");
    zmq_ctx_term = (zmq_ctx_term_t)GetProcAddress(libzmq, "zmq_ctx_term");
    if (zmq_ctx_new != NULL && zmq_socket != NULL && zmq_bind != NULL
        && zmq_connect != NULL && zmq_send != NULL && zmq_recv != NULL
        && zmq_close != NULL && zmq_ctx_term != NULL) {

        ctx = zmq_ctx_new();
        socket = zmq_socket(ctx, ZMQ_DEALER);
        std::string address = "tcp://*:" + port;
        zmq_bind(socket, address.c_str());
        islistening = true;
        return true;
    }
    else
        return false;
}

void send_message(std::string cmd_in, std::string data_in, bool blocking) {
	int flags = blocking ? 0 : ZMQ_DONWAIT;
    memcpy(cmd, cmd_in.data(), cmd_in.size());
    memcpy(data, data_in.data(), data_in.size());
    int res1 = zmq_send(socket, cmd, cmd_in.size(), flags | ZMQ_SNDMORE);
    int res2 = zmq_send(socket, data, data_in.size(), flags);
	if (res1 == -1 || res2 == -1) {
		std::cout << "One of the two messages failed" << endl;
	}
	else {
		std::cout << "Message sent" << std::endl;
	}
}

bool check_messages(CONTEXT* ctx, THREADID thread_id, bool blocking) {
	int flag = blocking ? 0 : ZMQ_DONWAIT;
	if (blocking) {
		std::cout << "Start reading blocking.." << std::endl;
	}
    if (islistening) {
        int len = zmq_recv(socket, &cmd, 10, flag);
        if (len > 0)  {
            int len2 = zmq_recv(socket, &data, 255, flag);
            if (len2 > 0) {
				return dispatch_messages(ctx, thread_id, std::string(cmd, len), std::string(data, len2));
            }
        }
    }
	return true;
}

void shutdown_server() {
    if (islistening) {
		zmq_close(socket);
		zmq_ctx_term(ctx);
        if (WINDOWS::FreeLibrary(libzmq))
            std::cout << "Library successfully freed" << std::endl;
        else
            std::cout << "Fail to free libzmq" << std::endl;
    }
}

#endif
