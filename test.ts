setInterval(async () => {
  const data = await fetch(
    "https://flightaware.com/ajax/trackpoll.rvt?token=dd1320656957446e9d96fc424a8f17988d3ec202332aa53a--8199400c044353d174dbdb70526036a03ca916ee&locale=en_US&summary=1",
    {
      "headers": {
        "accept": "*/*",
        "accept-language": "en-US,en;q=0.8",
        "cache-control": "no-cache",
        "pragma": "no-cache",
        "sec-ch-ua": '"Not?A_Brand";v="8", "Chromium";v="108", "Brave";v="108"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"macOS"',
        "sec-fetch-dest": "empty",
        "sec-fetch-mode": "cors",
        "sec-fetch-site": "same-origin",
        "sec-gpc": "1",
        "x-requested-with": "XMLHttpRequest",
      },
      "referrer": "https://flightaware.com/live/flight/VIV542",
      "referrerPolicy": "strict-origin-when-cross-origin",
      "body": null,
      "method": "GET",
      "mode": "cors",
      "credentials": "omit",
    },
  );

  const json = await data.json();
  const flights = json.flights;

  const keys = Object.keys(flights);
  const flight = json.flights[keys[0]];

  console.log(flight.flightStatus);

  // ["activityLog"]["flights"][0];
  // const originCoord = flight?.origin?.coord;
  // const destinationCoord = flight?.destination?.coord;

}, 3000);
