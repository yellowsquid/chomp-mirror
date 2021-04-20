use chewed::{IterWrapper, Parser};
use chomp_bench::json::*;
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    r#"{
  "id": 803,
  "main": "Clouds"
}"#,
    r#"{
  "id": 803,
  "main": "Clouds",
  "description": "broken clouds",
  "icon": "04d"
}"#,
    r#"{
  "temp": 273.52,
  "feels_like": 266.68,
  "temp_min": 273.17,
  "temp_max": 273.52,
  "pressure": 1009,
  "sea_level": 1009,
  "grnd_level": 997,
  "humidity": 81,
  "temp_kf": 0.35
}"#,
    r#"{
  "dt": 1612882800,
  "main": {
    "temp": 273.52,
    "feels_like": 266.68,
    "temp_min": 273.17,
    "temp_max": 273.52,
    "pressure": 1009,
    "sea_level": 1009,
    "grnd_level": 997,
    "humidity": 81,
    "temp_kf": 0.35
  },
  "weather": [
    {
      "id": 803,
      "main": "Clouds",
      "description": "broken clouds",
      "icon": "04d"
    }
  ]
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    }
  ]
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    },
    {
      "dt": 1612893600,
      "main": {
        "temp": 271.54,
        "feels_like": 264.77,
        "temp_min": 270.9,
        "temp_max": 271.54,
        "pressure": 1007,
        "sea_level": 1007,
        "grnd_level": 996,
        "humidity": 91,
        "temp_kf": 0.64
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 86
      },
      "wind": {
        "speed": 6.29,
        "deg": 63
      }
    }
  ],
  "city": {
    "id": 2654410,
    "name": "Buckingham",
    "coord": {
      "lat": 52,
      "lon": -1
    },
    "country": "GB",
    "population": 12791,
    "timezone": 0,
    "sunrise": 1612855819,
    "sunset": 1612890374
  }
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    },
    {
      "dt": 1612893600,
      "main": {
        "temp": 271.54,
        "feels_like": 264.77,
        "temp_min": 270.9,
        "temp_max": 271.54,
        "pressure": 1007,
        "sea_level": 1007,
        "grnd_level": 996,
        "humidity": 91,
        "temp_kf": 0.64
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 86
      },
      "wind": {
        "speed": 6.29,
        "deg": 63
      },
      "visibility": 10000,
      "pop": 0.08,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 18:00:00"
    },
    {
      "dt": 1612904400,
      "main": {
        "temp": 270.28,
        "feels_like": 263.77,
        "temp_min": 270.03,
        "temp_max": 270.28,
        "pressure": 1008,
        "sea_level": 1008,
        "grnd_level": 998,
        "humidity": 96,
        "temp_kf": 0.25
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.83,
        "deg": 65
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 21:00:00"
    },
    {
      "dt": 1612915200,
      "main": {
        "temp": 270.06,
        "feels_like": 263.35,
        "temp_min": 270.03,
        "temp_max": 270.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0.03
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 44
      },
      "wind": {
        "speed": 6.09,
        "deg": 56
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 00:00:00"
    }
  ],
  "city": {
    "id": 2654410,
    "name": "Buckingham",
    "coord": {
      "lat": 52,
      "lon": -1
    },
    "country": "GB",
    "population": 12791,
    "timezone": 0,
    "sunrise": 1612855819,
    "sunset": 1612890374
  }
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    },
    {
      "dt": 1612893600,
      "main": {
        "temp": 271.54,
        "feels_like": 264.77,
        "temp_min": 270.9,
        "temp_max": 271.54,
        "pressure": 1007,
        "sea_level": 1007,
        "grnd_level": 996,
        "humidity": 91,
        "temp_kf": 0.64
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 86
      },
      "wind": {
        "speed": 6.29,
        "deg": 63
      },
      "visibility": 10000,
      "pop": 0.08,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 18:00:00"
    },
    {
      "dt": 1612904400,
      "main": {
        "temp": 270.28,
        "feels_like": 263.77,
        "temp_min": 270.03,
        "temp_max": 270.28,
        "pressure": 1008,
        "sea_level": 1008,
        "grnd_level": 998,
        "humidity": 96,
        "temp_kf": 0.25
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.83,
        "deg": 65
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 21:00:00"
    },
    {
      "dt": 1612915200,
      "main": {
        "temp": 270.06,
        "feels_like": 263.35,
        "temp_min": 270.03,
        "temp_max": 270.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0.03
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 44
      },
      "wind": {
        "speed": 6.09,
        "deg": 56
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 00:00:00"
    },
    {
      "dt": 1612926000,
      "main": {
        "temp": 269.06,
        "feels_like": 262.65,
        "temp_min": 269.06,
        "temp_max": 269.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 18
      },
      "wind": {
        "speed": 5.51,
        "deg": 44
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 03:00:00"
    },
    {
      "dt": 1612936800,
      "main": {
        "temp": 268.96,
        "feels_like": 263.1,
        "temp_min": 268.96,
        "temp_max": 268.96,
        "pressure": 1012,
        "sea_level": 1012,
        "grnd_level": 1001,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 13
      },
      "wind": {
        "speed": 4.7,
        "deg": 36
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 06:00:00"
    },
    {
      "dt": 1612947600,
      "main": {
        "temp": 270.73,
        "feels_like": 264.69,
        "temp_min": 270.73,
        "temp_max": 270.73,
        "pressure": 1015,
        "sea_level": 1015,
        "grnd_level": 1004,
        "humidity": 95,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.2,
        "deg": 42
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 09:00:00"
    },
    {
      "dt": 1612958400,
      "main": {
        "temp": 273.21,
        "feels_like": 267.19,
        "temp_min": 273.21,
        "temp_max": 273.21,
        "pressure": 1017,
        "sea_level": 1017,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 30
      },
      "wind": {
        "speed": 5.51,
        "deg": 40
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 12:00:00"
    },
    {
      "dt": 1612969200,
      "main": {
        "temp": 273.22,
        "feels_like": 267.49,
        "temp_min": 273.22,
        "temp_max": 273.22,
        "pressure": 1018,
        "sea_level": 1018,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      }
    }
  ],
  "city": {
    "id": 2654410,
    "name": "Buckingham",
    "coord": {
      "lat": 52,
      "lon": -1
    },
    "country": "GB",
    "population": 12791,
    "timezone": 0,
    "sunrise": 1612855819,
    "sunset": 1612890374
  }
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    },
    {
      "dt": 1612893600,
      "main": {
        "temp": 271.54,
        "feels_like": 264.77,
        "temp_min": 270.9,
        "temp_max": 271.54,
        "pressure": 1007,
        "sea_level": 1007,
        "grnd_level": 996,
        "humidity": 91,
        "temp_kf": 0.64
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 86
      },
      "wind": {
        "speed": 6.29,
        "deg": 63
      },
      "visibility": 10000,
      "pop": 0.08,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 18:00:00"
    },
    {
      "dt": 1612904400,
      "main": {
        "temp": 270.28,
        "feels_like": 263.77,
        "temp_min": 270.03,
        "temp_max": 270.28,
        "pressure": 1008,
        "sea_level": 1008,
        "grnd_level": 998,
        "humidity": 96,
        "temp_kf": 0.25
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.83,
        "deg": 65
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 21:00:00"
    },
    {
      "dt": 1612915200,
      "main": {
        "temp": 270.06,
        "feels_like": 263.35,
        "temp_min": 270.03,
        "temp_max": 270.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0.03
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 44
      },
      "wind": {
        "speed": 6.09,
        "deg": 56
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 00:00:00"
    },
    {
      "dt": 1612926000,
      "main": {
        "temp": 269.06,
        "feels_like": 262.65,
        "temp_min": 269.06,
        "temp_max": 269.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 18
      },
      "wind": {
        "speed": 5.51,
        "deg": 44
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 03:00:00"
    },
    {
      "dt": 1612936800,
      "main": {
        "temp": 268.96,
        "feels_like": 263.1,
        "temp_min": 268.96,
        "temp_max": 268.96,
        "pressure": 1012,
        "sea_level": 1012,
        "grnd_level": 1001,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 13
      },
      "wind": {
        "speed": 4.7,
        "deg": 36
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 06:00:00"
    },
    {
      "dt": 1612947600,
      "main": {
        "temp": 270.73,
        "feels_like": 264.69,
        "temp_min": 270.73,
        "temp_max": 270.73,
        "pressure": 1015,
        "sea_level": 1015,
        "grnd_level": 1004,
        "humidity": 95,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.2,
        "deg": 42
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 09:00:00"
    },
    {
      "dt": 1612958400,
      "main": {
        "temp": 273.21,
        "feels_like": 267.19,
        "temp_min": 273.21,
        "temp_max": 273.21,
        "pressure": 1017,
        "sea_level": 1017,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 30
      },
      "wind": {
        "speed": 5.51,
        "deg": 40
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 12:00:00"
    },
    {
      "dt": 1612969200,
      "main": {
        "temp": 273.22,
        "feels_like": 267.49,
        "temp_min": 273.22,
        "temp_max": 273.22,
        "pressure": 1018,
        "sea_level": 1018,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 600,
          "main": "Snow",
          "description": "light snow",
          "icon": "13d"
        }
      ],
      "clouds": {
        "all": 62
      },
      "wind": {
        "speed": 5.11,
        "deg": 35
      },
      "visibility": 10000,
      "pop": 0.2,
      "snow": {
        "3h": 0.13
      },
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 15:00:00"
    },
    {
      "dt": 1612980000,
      "main": {
        "temp": 269.17,
        "feels_like": 264.29,
        "temp_min": 269.17,
        "temp_max": 269.17,
        "pressure": 1021,
        "sea_level": 1021,
        "grnd_level": 1010,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 31
      },
      "wind": {
        "speed": 3.34,
        "deg": 21
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 18:00:00"
    },
    {
      "dt": 1612990800,
      "main": {
        "temp": 268.37,
        "feels_like": 263.59,
        "temp_min": 268.37,
        "temp_max": 268.37,
        "pressure": 1023,
        "sea_level": 1023,
        "grnd_level": 1012,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3.05,
        "deg": 34
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 21:00:00"
    },
    {
      "dt": 1613001600,
      "main": {
        "temp": 267.89,
        "feels_like": 263.1,
        "temp_min": 267.89,
        "temp_max": 267.89,
        "pressure": 1024,
        "sea_level": 1024,
        "grnd_level": 1013,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3,
        "deg": 45
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 00:00:00"
    },
    {
      "dt": 1613012400,
      "main": {
        "temp": 267.84,
        "feels_like": 262.92,
        "temp_min": 267.84,
        "temp_max": 267.84,
        "pressure": 1025,
        "sea_level": 1025,
        "grnd_level": 1014,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3.17,
        "deg": 58
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 03:00:00"
    },
    {
      "dt": 1613023200,
      "main": {
        "temp": 267.92,
        "feels_like": 263.14,
        "temp_min": 267.92,
        "temp_max": 267.92,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 20
      },
      "wind": {
        "speed": 3.01,
        "deg": 88
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 06:00:00"
    },
    {
      "dt": 1613034000,
      "main": {
        "temp": 269.92,
        "feels_like": 264.79,
        "temp_min": 269.92,
        "temp_max": 269.92,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1016,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02d"
        }
      ],
      "clouds": {
        "all": 18
      },
      "wind": {
        "speed": 3.79,
        "deg": 118
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 09:00:00"
    },
    {
      "dt": 1613044800,
      "main": {
        "temp": 272.78,
        "feels_like": 266.21,
        "temp_min": 272.78,
        "temp_max": 272.78,
        "pressure": 1028,
        "sea_level": 1028,
        "grnd_level": 1017,
        "humidity": 93,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 56
      },
      "wind": {
        "speed": 6.28,
        "deg": 123
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 12:00:00"
    },
    {
      "dt": 1613055600,
      "main": {
        "temp": 272.43,
        "feels_like": 265.67,
        "temp_min": 272.43,
        "temp_max": 272.43,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1016,
        "humidity": 89,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 6.38,
        "deg": 124
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 15:00:00"
    },
    {
      "dt": 1613066400,
      "main": {
        "temp": 269.8,
        "feels_like": 263.76,
        "temp_min": 269.8,
        "temp_max": 269.8,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1015,
        "humidity": 93
      }
    }
  ],
  "city": {
    "id": 2654410,
    "name": "Buckingham",
    "coord": {
      "lat": 52,
      "lon": -1
    },
    "country": "GB",
    "population": 12791,
    "timezone": 0,
    "sunrise": 1612855819,
    "sunset": 1612890374
  }
}"#,
    r#"{
  "cod": "200",
  "message": 0,
  "cnt": 40,
  "list": [
    {
      "dt": 1612882800,
      "main": {
        "temp": 273.52,
        "feels_like": 266.68,
        "temp_min": 273.17,
        "temp_max": 273.52,
        "pressure": 1009,
        "sea_level": 1009,
        "grnd_level": 997,
        "humidity": 81,
        "temp_kf": 0.35
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 67
      },
      "wind": {
        "speed": 6.45,
        "deg": 78
      },
      "visibility": 10000,
      "pop": 0.12,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-09 15:00:00"
    },
    {
      "dt": 1612893600,
      "main": {
        "temp": 271.54,
        "feels_like": 264.77,
        "temp_min": 270.9,
        "temp_max": 271.54,
        "pressure": 1007,
        "sea_level": 1007,
        "grnd_level": 996,
        "humidity": 91,
        "temp_kf": 0.64
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 86
      },
      "wind": {
        "speed": 6.29,
        "deg": 63
      },
      "visibility": 10000,
      "pop": 0.08,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 18:00:00"
    },
    {
      "dt": 1612904400,
      "main": {
        "temp": 270.28,
        "feels_like": 263.77,
        "temp_min": 270.03,
        "temp_max": 270.28,
        "pressure": 1008,
        "sea_level": 1008,
        "grnd_level": 998,
        "humidity": 96,
        "temp_kf": 0.25
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.83,
        "deg": 65
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-09 21:00:00"
    },
    {
      "dt": 1612915200,
      "main": {
        "temp": 270.06,
        "feels_like": 263.35,
        "temp_min": 270.03,
        "temp_max": 270.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0.03
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 44
      },
      "wind": {
        "speed": 6.09,
        "deg": 56
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 00:00:00"
    },
    {
      "dt": 1612926000,
      "main": {
        "temp": 269.06,
        "feels_like": 262.65,
        "temp_min": 269.06,
        "temp_max": 269.06,
        "pressure": 1010,
        "sea_level": 1010,
        "grnd_level": 999,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 18
      },
      "wind": {
        "speed": 5.51,
        "deg": 44
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 03:00:00"
    },
    {
      "dt": 1612936800,
      "main": {
        "temp": 268.96,
        "feels_like": 263.1,
        "temp_min": 268.96,
        "temp_max": 268.96,
        "pressure": 1012,
        "sea_level": 1012,
        "grnd_level": 1001,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 13
      },
      "wind": {
        "speed": 4.7,
        "deg": 36
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 06:00:00"
    },
    {
      "dt": 1612947600,
      "main": {
        "temp": 270.73,
        "feels_like": 264.69,
        "temp_min": 270.73,
        "temp_max": 270.73,
        "pressure": 1015,
        "sea_level": 1015,
        "grnd_level": 1004,
        "humidity": 95,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 41
      },
      "wind": {
        "speed": 5.2,
        "deg": 42
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 09:00:00"
    },
    {
      "dt": 1612958400,
      "main": {
        "temp": 273.21,
        "feels_like": 267.19,
        "temp_min": 273.21,
        "temp_max": 273.21,
        "pressure": 1017,
        "sea_level": 1017,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03d"
        }
      ],
      "clouds": {
        "all": 30
      },
      "wind": {
        "speed": 5.51,
        "deg": 40
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 12:00:00"
    },
    {
      "dt": 1612969200,
      "main": {
        "temp": 273.22,
        "feels_like": 267.49,
        "temp_min": 273.22,
        "temp_max": 273.22,
        "pressure": 1018,
        "sea_level": 1018,
        "grnd_level": 1007,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 600,
          "main": "Snow",
          "description": "light snow",
          "icon": "13d"
        }
      ],
      "clouds": {
        "all": 62
      },
      "wind": {
        "speed": 5.11,
        "deg": 35
      },
      "visibility": 10000,
      "pop": 0.2,
      "snow": {
        "3h": 0.13
      },
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-10 15:00:00"
    },
    {
      "dt": 1612980000,
      "main": {
        "temp": 269.17,
        "feels_like": 264.29,
        "temp_min": 269.17,
        "temp_max": 269.17,
        "pressure": 1021,
        "sea_level": 1021,
        "grnd_level": 1010,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 31
      },
      "wind": {
        "speed": 3.34,
        "deg": 21
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 18:00:00"
    },
    {
      "dt": 1612990800,
      "main": {
        "temp": 268.37,
        "feels_like": 263.59,
        "temp_min": 268.37,
        "temp_max": 268.37,
        "pressure": 1023,
        "sea_level": 1023,
        "grnd_level": 1012,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3.05,
        "deg": 34
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-10 21:00:00"
    },
    {
      "dt": 1613001600,
      "main": {
        "temp": 267.89,
        "feels_like": 263.1,
        "temp_min": 267.89,
        "temp_max": 267.89,
        "pressure": 1024,
        "sea_level": 1024,
        "grnd_level": 1013,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3,
        "deg": 45
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 00:00:00"
    },
    {
      "dt": 1613012400,
      "main": {
        "temp": 267.84,
        "feels_like": 262.92,
        "temp_min": 267.84,
        "temp_max": 267.84,
        "pressure": 1025,
        "sea_level": 1025,
        "grnd_level": 1014,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 800,
          "main": "Clear",
          "description": "clear sky",
          "icon": "01n"
        }
      ],
      "clouds": {
        "all": 0
      },
      "wind": {
        "speed": 3.17,
        "deg": 58
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 03:00:00"
    },
    {
      "dt": 1613023200,
      "main": {
        "temp": 267.92,
        "feels_like": 263.14,
        "temp_min": 267.92,
        "temp_max": 267.92,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 97,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 20
      },
      "wind": {
        "speed": 3.01,
        "deg": 88
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 06:00:00"
    },
    {
      "dt": 1613034000,
      "main": {
        "temp": 269.92,
        "feels_like": 264.79,
        "temp_min": 269.92,
        "temp_max": 269.92,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1016,
        "humidity": 96,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02d"
        }
      ],
      "clouds": {
        "all": 18
      },
      "wind": {
        "speed": 3.79,
        "deg": 118
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 09:00:00"
    },
    {
      "dt": 1613044800,
      "main": {
        "temp": 272.78,
        "feels_like": 266.21,
        "temp_min": 272.78,
        "temp_max": 272.78,
        "pressure": 1028,
        "sea_level": 1028,
        "grnd_level": 1017,
        "humidity": 93,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 56
      },
      "wind": {
        "speed": 6.28,
        "deg": 123
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 12:00:00"
    },
    {
      "dt": 1613055600,
      "main": {
        "temp": 272.43,
        "feels_like": 265.67,
        "temp_min": 272.43,
        "temp_max": 272.43,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1016,
        "humidity": 89,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 6.38,
        "deg": 124
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-11 15:00:00"
    },
    {
      "dt": 1613066400,
      "main": {
        "temp": 269.8,
        "feels_like": 263.76,
        "temp_min": 269.8,
        "temp_max": 269.8,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1015,
        "humidity": 93,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 98
      },
      "wind": {
        "speed": 5,
        "deg": 106
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 18:00:00"
    },
    {
      "dt": 1613077200,
      "main": {
        "temp": 270.12,
        "feels_like": 263.78,
        "temp_min": 270.12,
        "temp_max": 270.12,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 93,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 97
      },
      "wind": {
        "speed": 5.48,
        "deg": 113
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-11 21:00:00"
    },
    {
      "dt": 1613088000,
      "main": {
        "temp": 270.55,
        "feels_like": 263.21,
        "temp_min": 270.55,
        "temp_max": 270.55,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 99
      },
      "wind": {
        "speed": 6.93,
        "deg": 116
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-12 00:00:00"
    },
    {
      "dt": 1613098800,
      "main": {
        "temp": 270.39,
        "feels_like": 263.71,
        "temp_min": 270.39,
        "temp_max": 270.39,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 90,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 99
      },
      "wind": {
        "speed": 5.94,
        "deg": 112
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-12 03:00:00"
    },
    {
      "dt": 1613109600,
      "main": {
        "temp": 270.92,
        "feels_like": 264.75,
        "temp_min": 270.92,
        "temp_max": 270.92,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 89,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 5.27,
        "deg": 110
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-12 06:00:00"
    },
    {
      "dt": 1613120400,
      "main": {
        "temp": 272.36,
        "feels_like": 264.88,
        "temp_min": 272.36,
        "temp_max": 272.36,
        "pressure": 1028,
        "sea_level": 1028,
        "grnd_level": 1017,
        "humidity": 87,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 7.33,
        "deg": 109
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-12 09:00:00"
    },
    {
      "dt": 1613131200,
      "main": {
        "temp": 274.01,
        "feels_like": 265.75,
        "temp_min": 274.01,
        "temp_max": 274.01,
        "pressure": 1030,
        "sea_level": 1030,
        "grnd_level": 1019,
        "humidity": 81,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 8.56,
        "deg": 111
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-12 12:00:00"
    },
    {
      "dt": 1613142000,
      "main": {
        "temp": 273.12,
        "feels_like": 265.01,
        "temp_min": 273.12,
        "temp_max": 273.12,
        "pressure": 1029,
        "sea_level": 1029,
        "grnd_level": 1018,
        "humidity": 87,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 8.37,
        "deg": 100
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-12 15:00:00"
    },
    {
      "dt": 1613152800,
      "main": {
        "temp": 269.85,
        "feels_like": 263.03,
        "temp_min": 269.85,
        "temp_max": 269.85,
        "pressure": 1031,
        "sea_level": 1031,
        "grnd_level": 1019,
        "humidity": 92,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 803,
          "main": "Clouds",
          "description": "broken clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 66
      },
      "wind": {
        "speed": 6.1,
        "deg": 95
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-12 18:00:00"
    },
    {
      "dt": 1613163600,
      "main": {
        "temp": 269.25,
        "feels_like": 263.22,
        "temp_min": 269.25,
        "temp_max": 269.25,
        "pressure": 1032,
        "sea_level": 1032,
        "grnd_level": 1021,
        "humidity": 95,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 801,
          "main": "Clouds",
          "description": "few clouds",
          "icon": "02n"
        }
      ],
      "clouds": {
        "all": 11
      },
      "wind": {
        "speed": 4.95,
        "deg": 106
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-12 21:00:00"
    },
    {
      "dt": 1613174400,
      "main": {
        "temp": 269.56,
        "feels_like": 263.68,
        "temp_min": 269.56,
        "temp_max": 269.56,
        "pressure": 1032,
        "sea_level": 1032,
        "grnd_level": 1021,
        "humidity": 93,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 802,
          "main": "Clouds",
          "description": "scattered clouds",
          "icon": "03n"
        }
      ],
      "clouds": {
        "all": 26
      },
      "wind": {
        "speed": 4.74,
        "deg": 127
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-13 00:00:00"
    },
    {
      "dt": 1613185200,
      "main": {
        "temp": 269.58,
        "feels_like": 263.32,
        "temp_min": 269.58,
        "temp_max": 269.58,
        "pressure": 1032,
        "sea_level": 1032,
        "grnd_level": 1021,
        "humidity": 90,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 5.22,
        "deg": 124
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-13 03:00:00"
    },
    {
      "dt": 1613196000,
      "main": {
        "temp": 269.58,
        "feels_like": 262.99,
        "temp_min": 269.58,
        "temp_max": 269.58,
        "pressure": 1032,
        "sea_level": 1032,
        "grnd_level": 1021,
        "humidity": 91,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 5.71,
        "deg": 123
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-13 06:00:00"
    },
    {
      "dt": 1613206800,
      "main": {
        "temp": 270.54,
        "feels_like": 263.25,
        "temp_min": 270.54,
        "temp_max": 270.54,
        "pressure": 1033,
        "sea_level": 1033,
        "grnd_level": 1022,
        "humidity": 88,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 6.79,
        "deg": 128
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-13 09:00:00"
    },
    {
      "dt": 1613217600,
      "main": {
        "temp": 272.82,
        "feels_like": 265.24,
        "temp_min": 272.82,
        "temp_max": 272.82,
        "pressure": 1033,
        "sea_level": 1033,
        "grnd_level": 1022,
        "humidity": 84,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 95
      },
      "wind": {
        "speed": 7.47,
        "deg": 127
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-13 12:00:00"
    },
    {
      "dt": 1613228400,
      "main": {
        "temp": 272.61,
        "feels_like": 265.29,
        "temp_min": 272.61,
        "temp_max": 272.61,
        "pressure": 1032,
        "sea_level": 1032,
        "grnd_level": 1021,
        "humidity": 83,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 99
      },
      "wind": {
        "speed": 7.04,
        "deg": 128
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-13 15:00:00"
    },
    {
      "dt": 1613239200,
      "main": {
        "temp": 271.76,
        "feels_like": 264.27,
        "temp_min": 271.76,
        "temp_max": 271.76,
        "pressure": 1031,
        "sea_level": 1031,
        "grnd_level": 1020,
        "humidity": 83,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 600,
          "main": "Snow",
          "description": "light snow",
          "icon": "13n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 7.15,
        "deg": 129
      },
      "visibility": 9074,
      "pop": 0.36,
      "snow": {
        "3h": 0.38
      },
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-13 18:00:00"
    },
    {
      "dt": 1613250000,
      "main": {
        "temp": 272.69,
        "feels_like": 264.96,
        "temp_min": 272.69,
        "temp_max": 272.69,
        "pressure": 1031,
        "sea_level": 1031,
        "grnd_level": 1020,
        "humidity": 84,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 7.67,
        "deg": 138
      },
      "visibility": 10000,
      "pop": 0.6,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-13 21:00:00"
    },
    {
      "dt": 1613260800,
      "main": {
        "temp": 272.66,
        "feels_like": 265.14,
        "temp_min": 272.66,
        "temp_max": 272.66,
        "pressure": 1030,
        "sea_level": 1030,
        "grnd_level": 1019,
        "humidity": 87,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 600,
          "main": "Snow",
          "description": "light snow",
          "icon": "13n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 7.45,
        "deg": 143
      },
      "visibility": 5551,
      "pop": 0.64,
      "snow": {
        "3h": 0.25
      },
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-14 00:00:00"
    },
    {
      "dt": 1613271600,
      "main": {
        "temp": 272.3,
        "feels_like": 265.08,
        "temp_min": 272.3,
        "temp_max": 272.3,
        "pressure": 1029,
        "sea_level": 1029,
        "grnd_level": 1018,
        "humidity": 89,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 600,
          "main": "Snow",
          "description": "light snow",
          "icon": "13n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 7.01,
        "deg": 143
      },
      "visibility": 10000,
      "pop": 0.24,
      "snow": {
        "3h": 0.13
      },
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-14 03:00:00"
    },
    {
      "dt": 1613282400,
      "main": {
        "temp": 272.19,
        "feels_like": 265.26,
        "temp_min": 272.19,
        "temp_max": 272.19,
        "pressure": 1027,
        "sea_level": 1027,
        "grnd_level": 1016,
        "humidity": 90,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04n"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 6.6,
        "deg": 139
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "n"
      },
      "dt_txt": "2021-02-14 06:00:00"
    },
    {
      "dt": 1613293200,
      "main": {
        "temp": 273.32,
        "feels_like": 265.28,
        "temp_min": 273.32,
        "temp_max": 273.32,
        "pressure": 1026,
        "sea_level": 1026,
        "grnd_level": 1015,
        "humidity": 85,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 8.25,
        "deg": 136
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-14 09:00:00"
    },
    {
      "dt": 1613304000,
      "main": {
        "temp": 275.38,
        "feels_like": 267.23,
        "temp_min": 275.38,
        "temp_max": 275.38,
        "pressure": 1025,
        "sea_level": 1025,
        "grnd_level": 1014,
        "humidity": 75,
        "temp_kf": 0
      },
      "weather": [
        {
          "id": 804,
          "main": "Clouds",
          "description": "overcast clouds",
          "icon": "04d"
        }
      ],
      "clouds": {
        "all": 100
      },
      "wind": {
        "speed": 8.46,
        "deg": 143
      },
      "visibility": 10000,
      "pop": 0,
      "sys": {
        "pod": "d"
      },
      "dt_txt": "2021-02-14 12:00:00"
    }
  ],
  "city": {
    "id": 2654410,
    "name": "Buckingham",
    "coord": {
      "lat": 52,
      "lon": -1
    },
    "country": "GB",
    "population": 12791,
    "timezone": 0,
    "sunrise": 1612855819,
    "sunset": 1612890374
  }
}"#,
];

fn parse_chewed(input: &str) -> Value {
    IterWrapper::new(input.chars())
        .parse::<nibble::Ast>()
        .unwrap()
        .into()
}

fn parse_handwritten(input: &str) -> Value {
    IterWrapper::new(input.chars()).parse().unwrap()
}

fn parse_lalrpop(parser: &lalr::ValueParser, input: &str) -> Value {
    parser.parse(input).unwrap()
}

fn bench_parse(c: &mut Criterion) {
    let lalr_parser = lalr::ValueParser::new();
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("JSON");
    group.plot_config(plot_config);
    for (i, input) in INPUTS.iter().enumerate() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("Chewed", i), *input, |b, i| {
            b.iter(|| parse_chewed(i))
        });
        group.bench_with_input(BenchmarkId::new("Handwritten", i), *input, |b, i| {
            b.iter(|| parse_handwritten(i))
        });
        group.bench_with_input(BenchmarkId::new("LALRPOP", i), *input, |b, i| {
            b.iter(|| parse_lalrpop(&lalr_parser, i))
        });
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
