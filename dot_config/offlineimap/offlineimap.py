#! /usr/bin/env python3
from subprocess import check_output

# To store password
# secret-tool store --label "Fastmail IMAP" host imaps-proxy.fastmail.com
def get_pass(account):
    return check_output("secret-tool lookup host imaps-proxy.fastmail.com", shell=True).splitlines()[0]
