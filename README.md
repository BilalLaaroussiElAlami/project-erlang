# Microblogging service

*Multicore Programming 2023 - Erlang Project*

most recent code is in branch push-based-model ! 


I implemented and evaluated a decentralized microblogging application, like twitter. 
Users can connect to the application, send messages, follow each other, and read each other’s messages. The application consists of multiple “instances”: separate Erlang processes that are responsible for a subset of the users. Users can follow each other and receive messages across instances. Finally, I evaluated the system with a set of benchmarks, simulating a variety of workloads. The system is scalable and adapts to large workloads. In this project I learned a lot about scalability and consistency of big systems , and how those two characteristics can be a tradeoff. The project was written in Erlang, a programming language primarily known for its concurrency, fault tolerance, and scalability features, often used in building distributed, fault-tolerant systems.


