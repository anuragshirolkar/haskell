module Rpc where


class Rpc req res where
    execute :: req -> res