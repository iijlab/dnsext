# PR の実例集：まずかった PR と、やり直し後の形

`CLAUDE.md` の「PR と差分の作り方」「コミットメッセージ」「既知の逸脱」の各規則について、実際の PR での例を集めたもの。
規則の本体は `CLAUDE.md` にあり、ここは読み物。番号は iijlab/dnsext の PR 番号。
やり直しが進んで「悪い例」の実体が消えたら、該当する項目は削ってよい。

## 差分

### 1 つの PR で 1 つの話題

- #284（DNS-0x20）は「表記を折り畳むサーバへの問い合わせ直し」と「既定値を on にする運用判断」を 1 つの PR にした。「落とせるように別コミットにした」とあるが、運用上の判断は別 PR にする。
- #257（ECDSA の整数を曲線幅に）は、RFC 6605 §4 が対象とする公開鍵と署名に加えて、要求外の秘密鍵ファイルの形式まで変えた。やり直し（bp-rearrange-257）は公開鍵と署名だけ。
- #253 は doctest から外部依存を外す話に、bogus 判定のテスト追加という別の話題を足した。

### 差分を最小に

- #253（fillDelegationDS の doctest）は、外部に問い合わせないようにするだけの修正に、応答表を運ぶ ReaderT、noDS / signedDS の 2 表、isBogus 判定を足して +35/-4 になった。やり直し（rearrange-253）は +9/-37。
- #257 は本体 +27 に対しテスト +67（先頭ゼロの鍵 4 本、署名 500 回 × 2 曲線）。やり直しは ECDSA.hs の +10/-3 だけ。

### 症状でなく原因を

- #253 の doctest が壊れた原因は、実在ドメイン（mew.org / org.）を例に使い、mew.org が DS を持った日に答えが変わったこと。元の PR は名前を残して応答だけ差し替えた。やり直しは example. / 192.0.2.x に替えて原因を断った。

### 例示用の空間

- 上の #253。やり直し後は `example.`、`a.example.`、`192.0.2.1`〜`192.0.2.4`。

### テストのための export と別ファイルのテストを作らない

- #266（RRset の TTL は最小値）は本体 1 行の修正に対して test/RRsetSpec.hs（36 行）を新設し、cabal に追加した。既存の doctest で足りる。
- #303（最上位ビット TTL）と #304（保持上限）は Internal.hs の export を増やし、test/TTLSpec.hs（56 行）と test/MaxCacheTTLSpec.hs（57 行）を新設した。元になった #265 と同じ形。
- #268（verifyRRSIG のワイルドカード）は、使用例として残している API（verifyRRSIG）に 3 件のテストと RSA 鍵定数の括り出しを足した。本体の経路は verifyRRSIGsorted なので意味が薄く、マージ時に全部戻した。

### 既存モジュールの役割を広げない

- #277（DNS-0x20）は識別子生成のモジュール `DNS.Do53.Id` を「推測されにくい部分の生成元」に広げ、`nextId` を `nextBytes` に一般化して、名前の case を混ぜる処理を同居させた。識別子の話（#274）と 0x20 の話が同じモジュールに混ざった。

## コメントと Haddock

### 経緯はコミットメッセージへ

- #257 の `fixedWidth` の Haddock は、RFC の節、`i2osp` の性質、256 分の 1 の確率、切り詰めが起きない議論で 11 行あった。やり直しは `-- RFC 6605 requires each ECDSA integer to be encoded at curve width.` の 1 行。
- #268 の RFC 4035 §5.3.2 の引用と MUST NOT の説明 6 行は、やり直しで `-- For wildcard case` の 1 行になった。Haddock は「verifyRRSIG は verifyRRSIGsorted の使用例としてのみ存在する」と API の位置づけだけを書く形に。
- #253 の Haddock には「What the parent says is not the internet's to decide any more.」のような段落が 3 つあった。やり直しでは全部無い。
- #277 の `DNS.Do53.Id` 冒頭 Haddock（15 行）、`mixCase`（10 行）、`checkRespM'`（4 行）、#284 の再送箇所（5 行 × 2）。

### 同じコメントを繰り返さない

- #275（応答パケットには答えない）は "A response is not a question, and answering one is how two servers which both do it end up talking to each other..." という段落を、bowline、clove、cacher の 3 箇所にほぼ同文で書いた。1 行で足りる。

## PR の説明

### よい例（人が書いた PR）

- バグの修正：#196（TXT の複数文字列が保たれない）。タイトルが問題そのもので、本文は dig と dug の出力を並べた証拠と、原因になったコミットへのリンクだけ。
- バグの修正：#178（VC の送信スレッドが終わらない）。問題の要約 1 文、`tstats` の出力、終了条件の 3 点。
- バグの修正：#127（DS を補う処理が余計に呼ばれる）。要約、`Fix #126` に当たる 1 行、直ったあとの dug の出力。
- 機能追加：#229（dug の `-f short`）。その機能で動くようになるスクリプトだけ。
- 機能追加：#223（SVCB の表示）。1 行の要約と、Current / New の出力の対比。

### 長すぎた例

- #322、#327（clove の応答の切り詰め）。冒頭 2〜3 行の要約で足りているのに、「どこがどう間違っていたか／どう直したか／どの層／どう確かめたか」の見出しと表が続く。冒頭の要約と、main での再現（`dig` の出力）だけでよい。
- #304（`cache-max-ttl`）。4 点の箇条書きに、変えた関数の一覧と、変えなかったことまで書いてある。要約と、設定の書き方の例だけでよい。

### 根拠の書き方

- #284 のコミット本文に「BIND、NSD、Knot はみな名前をそのまま返す」とあるが、確かめた方法が書かれていない。

### 積み重ねた PR

- #299（証明の無い DS 不在）の doctest は、#253 のやり直し前の形（`runChild noDS` と ReaderT の応答表）に依存していた。main が rearrange-253 で戻ったあと噛み合わなくなり、rebase と doctest の作り直しが要る。
- #296 は #295 の上に積んである。概要にその旨と番号を書くのはよい。

## コミットメッセージ

### 件名の接頭辞

- 接頭辞の無い件名：#275 `a response is not a question, and is not answered`、#277 `mixing the case of a query, and checking it comes back (DNS-0x20)`、#285 `read on what somebody typed, and a number which has to fit`。

### 本文

- #257 の本文は英語で、clove での実測ログ、確率計算、無関係な dnsext-do53 のテスト失敗の話まで含む。
- #258 は修正 1 行に対し、実測表とログ付きの英語の本文。
- #271 は修正 1 行に対し、実測ログ付きの英語の本文。コメントはマージ時に `bowline: simplify comment.` で手直しした。

### やり直しの分け方

- rearrange-253 の 3 コミット：`doctest: fillDelecationDS: revert #253 to rearrange` → `doctest: fillDelegationDS: minimum fix for dummy parent` → `doctest: fillDelegationDS: switch to example data`。件名だけで本文なし。

## 既知の逸脱：DNS-0x20（#277、#284）が原則からずれている点

`CLAUDE.md` の「Norec 以下は 1 回送って 1 回受け取るだけ」に照らして。

- `Norec` でクエリ（名前の表記）を書き換えている。`norec_` が `mixCase_` で名前を混ぜてから Question を組む。表記を決めるのは `ResolveJust` の役目。
- `Do53Stub` が応答を見て、クエリを変えて同じサーバに送り直している（#284 の `udpResolver1` / `vcResolver1`）。許されている例外は「同じクエリの TCP フォールバック 1 回」で、クエリを変える再送は当たらない。
- `checkResp` に `mixed :: Bool` を足し、`isJust ractionMixCase` を各所で見ている。照合の方針は `checkResp` の中に閉じ、`Do53Stub` が上位の方針を知らずに済む形にする。
- UDP の `recvAnswer` の「照合に通らないパケットを捨てて受信を続ける」ループ（これ自体が既知の逸脱）に、`foldedTheCase` の分岐を 1 本足して複雑にした。
- 反復検索のための実装とテスト（`mixCase`、`sameCase`、`newConcurrentMixCase`、`ractionMixCase`、test/MixCaseSpec.hs）を `dnsext-do53` に置いた。do53 はスタブ、DoX、clove でも使われる。
- RFC 4343 §3 はサーバが受け取った表記で名前を返すことを求めているが、「返さないサーバの応答を捨てる」のは実装の選択。#277 の説明とコメントはその区別を付けていない。DNS-0x20 自体は draft が失効していて RFC ではない。
- #284 は「折り畳むサーバを覚えない」と選び、そのサーバに毎回 2 往復かける。理由付け（覚えると ID を 1 回当てた攻撃者に恒久的な降格を許す）は妥当だが、結果として `Do53Stub` の中で 2 回送る設計になった。原則の立場は、失敗を早く返して `ResolveJust` が別の NS や別のクエリで問い合わせ直すこと。

やり直しの方向：#277 / #284 を巻き戻し、表記を混ぜるのも、表記が合わないサーバへの問い合わせ直しも `ResolveJust` に置く。`Do53Stub` には TCP 切り替えと同じ形で判定関数だけを渡す。`dnsext-do53` には手を入れない。
