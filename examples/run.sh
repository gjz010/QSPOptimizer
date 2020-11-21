RUST_LOG=trace RUST_BACKTRACE=1 cargo run --release -- --preprocess --optimize-only <$1  > $1.log  2>&1
echo $1: $?