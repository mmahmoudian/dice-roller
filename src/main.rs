use iced::widget::{button, column, row, text};
use iced::{window, Element, Sandbox, Settings};
mod roll_logic;

struct DiceRoller {
    // should be the dice roll
    dice: i32,
    result: (String, String),
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    IncrementPressed,
    DecrementPressed,
    Roll,
    Extended,
    Simple,
}

impl Sandbox for DiceRoller {
    type Message = Message;

    fn new() -> Self {
        Self {
            dice: 0,
            result: (String::from(""), String::from("")),
        }
    }

    fn title(&self) -> String {
        String::from("Test dice roller")
    }

    fn view(self: &DiceRoller) -> Element<Message> {
        // vertical layout
        column![
            //horizontal layout for buttons in first row
            row![
                // increment number of dice on press
                button("+").on_press(Message::IncrementPressed),
                // show number of dice to roll in between buttons
                text(self.dice).size(25),
                // decrement number of dice
                button("-").on_press(Message::DecrementPressed),
                // Perform the roll
                button("Roll").on_press(Message::Roll),
                // Perform extended test
                button("Extended").on_press(Message::Extended),
                // Just a simple roll
                button("Simple").on_press(Message::Simple),
            ]
            .spacing(20),
            row![text(&self.result.0).size(25), text(&self.result.1).size(25),].spacing(20)
        ]
        .into()
    }

    fn update(&mut self, message: Message) {
        let mut round_res = Vec::new();
        let mut round_notes = Vec::new();
        let glitch_indicator = String::from("!");
        let mut totals = vec![0, 0, 0];
        match message {
            Message::IncrementPressed => {
                self.dice += 1;
            }
            Message::DecrementPressed => {
                if self.dice > 0 {
                    self.dice -= 1;
                }
            }
            Message::Roll => {
                if self.dice > 0 {
                    round_res.push(String::from("Hit/Miss/Total"));
                    round_notes.push(String::from("Glitch Status"));
                    let r = roll_logic::roll(self.dice, 6);
                    let res = roll_logic::simple_print(&r);
                    round_res.push(res.0);
                    round_notes.push(res.1);
                }
            }
            Message::Extended => {
                if self.dice > 0 {
                    let mut rounds = self.dice;

                    round_res.push(String::from("Hit/Miss/Total"));
                    round_notes.push(String::from("Glitch Status"));
                    while rounds > 0 {
                        let r = roll_logic::roll(rounds, 6);
                        let res = roll_logic::roll_result(&r);
                        let gs = res.1.clone();
                        totals[0] += &res.0 .0;
                        totals[1] += &res.0 .1;
                        totals[2] += &res.0 .2;
                        round_res.push(format!("{}/{}/{}", res.0 .0, res.0 .1, res.0 .2));
                        round_notes.push(res.1);
                        if gs.contains(&glitch_indicator) {
                            self.dice = rounds;
                            break;
                        }
                        rounds -= 1;
                    }
                    round_res.push(String::from(""));
                    round_res.push(format!("Total: {}/{}/{}", totals[0], totals[1], totals[2]));
                    round_notes.push(String::from(""));
                    round_notes.push(String::from(""));
                }
            }
            Message::Simple => {
                let r = roll_logic::roll(self.dice, 6);
                round_res.push(String::from("Sides"));
                round_res.push(String::from("Value"));
                round_notes.push(String::from("1 2 3 4 5 6"));
                round_notes.push(
                    r.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<String>>()
                        .join(" "),
                );
            }
        }

        self.result = (round_res.join("\n"), round_notes.join("\n"));
    }
}

pub fn main() -> iced::Result {
    let settings = Settings {
        window: window::Settings {
            size: (400, 600),
            resizable: true,
            decorations: true,
            ..Default::default()
        },
        ..Default::default()
    };
    DiceRoller::run(settings)
}
