exports.dec2bin = function dec2bin(dec) {
  return (dec >>> 0).toString(2);
}

exports.bin2dec = function bin2dec(bin) {
  return parseInt(bin, 2) >> 0;
}
