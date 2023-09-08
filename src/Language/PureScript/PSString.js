export const decodeUtf16BEImpl = (arrayOfUt16CodeUnits) => {
  const arrayBuf = new ArrayBuffer(2 * arrayOfUt16CodeUnits.length);
  const dataBuf = new DataView(arrayBuf);
  for (const i = 0; i < arrayOfUt16CodeUnits.length; i++) {
    dataBuf.setUint16(2 * i, arrayOfUt16CodeUnits[i], false);
  }
  const decoder = new TextDecoder("utf-16be", { fatal: true, ignoreBOM: false });
  try {
    return decoder.decode(dataBuf);
  } catch {
    return null;
  }
}
