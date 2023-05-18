module Duration = {
  let secondInMs = 1000.
  let minuteInMs = 60. *. secondInMs
  let hourInMs = 60. *. minuteInMs
  let dayInMs = 24. *. hourInMs
  let monthInMs = 30. *. dayInMs
}

module Auth = {
  let tokenCookieName = "jwtToken"
}
