################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/erl_comm.c 

CPP_SRCS += \
../src/Erlang-proto-player-cc.cpp 

OBJS += \
./src/Erlang-proto-player-cc.o \
./src/erl_comm.o 

C_DEPS += \
./src/erl_comm.d 

CPP_DEPS += \
./src/Erlang-proto-player-cc.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -I/usr/X11R6/include -I/opt/local/include/player-2.0 -I/opt/local/include -I/opt/local/lib/erlang/lib/erl_interface-3.5.9/include/ -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/%.o: ../src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -I/usr/X11R6/include -I/opt/local/include/player-2.0 -I/opt/local/include -I/opt/local/lib/erlang/lib/erl_interface-3.5.9/include/ -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


