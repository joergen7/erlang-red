## Raspberry Pi 4B

Notes on how to I got Erlang-Red running on a Raspberry Pi. This uses docker so it isn't something that will work on a mini and certainly your mileage will vary.

I first burnt the latest Raspberry Pi OS 64-bit (2025-05-13) onto an SD-Card.

I used a docker image because I did not want to upgrade Erlang on the Raspberry OS - it comes in at 25 but erlexec requires higher - or better said that's where the compiling stopped. In hindsight it was probably something else related to the network mount I was using.

Either way docker provides the isolation that is always good for developing projects. To get docker working, I followed these steps:

1. Install docker

https://raspberrytips.com/docker-on-raspberry-pi/

> curl -sSL https://get.docker.com | sh

Allow a user to use docker:

> sudo usermod -aG docker $USER

2. Switch on I2C bus

https://pi3g.com/enabling-and-checking-i2c-on-the-raspberry-pi-using-the-command-line-for-your-own-scripts/

> sudo raspi-config nonint do_i2c 0

3. Change permissions on the I2C device

https://stackoverflow.com/questions/24225647/docker-a-way-to-give-access-to-a-host-usb-or-serial-device/57117369#57117369

> Create a file named /etc/udev/rules.d/99-serial.rules. Add the following line to that file:

> KERNEL=="ttyUSB[0-9]*",MODE="0666"

4. Allow docker to access device

> --device=/dev/i2c-1

> docker run -it -v $(pwd):/code -v $(pwd)/data:/data --device=/dev/i2c-1 -p 9090:8080 -w /code --rm erlang-shell bash

or [make start-docker-shell-raspberry](Makefile).

5. Inside the docker image, check the access to the i2c device by doing:

> i2cdetect 1

The primary device should be on bus 1.

The [Dockerfile.dev](Dockerfile.dev) has i2c-tools included and these in turn are required by circuits_i2c.


After that it's `make app-start-loop` inside the docker image to start the server.
