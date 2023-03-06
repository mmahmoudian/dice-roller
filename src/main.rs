use iced::widget::{button, column, row, text};
use iced::{Sandbox, Settings, Element};

mod roll_logic;

struct DiceRoller {
    // should be the dice roll
    dice: i32,
    result: String,
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
            result: String::from("")
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
            ].spacing(10),
            // Roll result as next row
            text(&self.result).size(25),
        ].into()
    }

    fn update(&mut self, message: Message) {
        match message {
            Message::IncrementPressed => {
                self.dice += 1;
            }
            Message::DecrementPressed => {
                self.dice -= 1;
            }
            Message::Roll => {
                let res = roll_logic::roll(self.dice, 6);
                self.result = roll_logic::simple_print(&res);
            }
            Message::Extended => {
                let mut rounds = self.dice;
                let mut round_res = Vec::new();
                while rounds > 0{
                    let r = roll_logic::roll(rounds, 6);
                    let res = roll_logic::simple_print(&r);
                    round_res.push(res);
                    rounds -= 1;
                }
                self.result = round_res.join("\n");
            }
        }
    }
}

pub fn main() -> iced::Result {
    DiceRoller::run(Settings::default())
}
