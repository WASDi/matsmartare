export default function formatPrice(price) {
  if (typeof(price) === 'number' && price % 1 !== 0) {
    return price.toFixed(2);
  }
  return price + ":-";
}
