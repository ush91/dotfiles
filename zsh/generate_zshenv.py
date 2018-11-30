#!/usr/bin/env python3

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-p', '--path',
                    help='Paths to add to $PATH',
                    nargs='+',
                    type=str)
parser.add_argument('-z', '--zdotdir',
                    help='Path of $ZDOTDIR',
                    nargs=1,
                    type=str)

if __name__ == '__main__':
    args = parser.parse_args()

    print('#')
    print('# This file is generated by \'{0}\''.format(os.path.abspath(__file__)))
    print('#')

    if (args.zdotdir):
        print()
        print('export ZDOTDIR={0}'.format(args.zdotdir[0]))

    if (args.path):
        print()
        print('typeset -U path')
        for path in args.path:
            print('path+=({0}(N-/))'.format(path))

        print('export PATH')
