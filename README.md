ふつうのHaskellプログラミング サンプルコード
================

サンプルコードをGHC 7.6.3に対応させました。

主な変更点

* importのモジュール構造の変更に対応(List -> Data.Listなど)
* UTCTime対応
* それに伴い、モジュール名とかぶるので、POSIXプラグマを_POSIXに
* 名前が微妙にかわった関数名対応(catch -> catchIOError)
* LazyLinesのIOにエンコーディングを付ける

オリジナルコードは、下記からダウンロードしました。

http://i.loveruby.net/ja/stdhaskell/


