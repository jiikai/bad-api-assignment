module.exports = (req, res, next) => {
  res.header('Access-Control-Expose-Headers', 'ETag')
  res.header('X-Error-Modes-Active', '')
  //  Math.random() < 0.5 ? '' : 'availability-empty'
  //)
  next()
}
