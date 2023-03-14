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
            ]
            .spacing(20),
            row![text(&self.result.0).size(25), text(&self.result.1).size(25),].spacing(20)
        ]
        .into()
    }

    fn update(&mut self, message: Message) {
        let mut round_res = Vec::new();
        let mut round_notes = Vec::new();
        round_res.push(String::from("Hit/Miss/Total"));
        round_notes.push(String::from("Glitch Status"));
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
                    let r = roll_logic::roll(self.dice, 6);
                    let res = roll_logic::simple_print(&r);
                    round_res.push(res.0);
                    round_notes.push(res.1);
                }
            }
            Message::Extended => {
                let mut rounds = self.dice;
                while rounds > 0 {
                    let r = roll_logic::roll(rounds, 6);
                    let res = roll_logic::simple_print(&r);
                    round_res.push(res.0);
                    round_notes.push(res.1);
                    rounds -= 1;
                }
            }
        }
        self.result = (round_res.join("\n"), round_notes.join("\n"));
    }
}

pub fn main() -> iced::Result {
    let settings = Settings {
        window: window::Settings {
            size: (300, 600),
            resizable: true,
            decorations: true,
            ..Default::default()
        },
        ..Default::default()
    };
    DiceRoller::run(settings)
}
