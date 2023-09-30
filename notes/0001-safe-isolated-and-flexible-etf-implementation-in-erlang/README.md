---
date: 2023-09-30
title: Berty, a Safe, Isolated and Flexible ETF Implementation in Erlang
subtitle: Improving Erlang Term Format from Scratch
author: Mathieu Kerjouan
keywords: erlang,otp,etf,erlang term format,bert,bert
license: CC BY-NC-ND
abstract: |
  10 years ago, BERT, for Binary ERlang Term, was a serialized formation
  used at Github or in some other big places. This binary format is easy
  to understand, easy to parse and to test. Some problems remains hard
  to fix though, even after many years of development and improvement,
  the BEAM (Erlang Virtual Machine) is still vulnerable to atom or
  memory exhaustion. Despite the high quality of Erlang/OTP and its vast
  adoption in highly distributed environment, these errors are still a
  risk for services and developers.

  
toc: true
hyperrefoptions:
  - linktoc=all
---

# Berty, a Safe, Isolated and Flexible ETF Implementation in Erlang

## Improving Erlang Term Formation from Scratch

10 years ago, BERT, for Binary ERlang Term, was a serialized formation
used at Github or in some other big places. This binary format is easy
to understand, easy to parse and to test. Some problems remains hard
to fix though, even after many years of development and improvement,
the BEAM (Erlang Virtual Machine) is still vulnerable to atom or
memory exhaustion. Despite the high quality of Erlang/OTP and its vast
adoption in highly distributed environment, these errors are still a
risk for services and developers.

# Introduction

Say you want to implement a chat service quick, with only few Erlang
dependencies. By looking on the toolbox delivered with Erlang/OTP, you
will find everything you need. If you want to create a chat service
over UDP, TCP, SSH or HTTP, an application is available. If you want
to craft your own low level textual syntax, you have access to Leex or
Yeec. If you want to communicate with an optimized and already
compatible format, you will use ETF.

# Denial of Services and the BEAM

 - atom exhaustion
 - memory exhaustion 
 - denial of services (CPU, memory usage)
 
# ETF, BERT and Berty

## Safety

## Flexibility

## Isolation

# Conclusion
