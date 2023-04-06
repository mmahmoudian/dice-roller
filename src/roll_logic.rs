use rand::prelude::*;
//use std::io::Write;

pub fn roll(n: i32, sides: i32) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    let mut roll = vec![0; sides as usize];
    for _i in 0..n {
        let roll_result = rng.gen_range(1..(sides + 1));
        roll[(roll_result - 1) as usize] += 1;
    }

    return roll
}

pub fn simple_print(res: &Vec<i32>) -> (String, String) {
    let hits = count_hits(res);
    let misses = res[0];
    let total: i32 = res.iter().sum();
    let miss_percentage: f32 = res[0] as f32 / total as f32;
    let diff = hits - misses;
    let mut status: String = String::from("");

    if diff < 0 {
        status += "!";
        if miss_percentage >= 0.5 {
            status += "!"
        }
    }




    return (format!("{}/{}/{}", hits, misses, total), status);
}

pub fn roll_result(res: &Vec<i32>) -> ((i32, i32, i32), String) {
    let hits = count_hits(res);
    let misses = res[0];
    let total: i32 = res.iter().sum();
    let miss_percentage: f32 = res[0] as f32 / total as f32;
    let diff = hits - misses;
    let mut status: String = String::from("");
    let result = (hits, misses, total);

    if diff < 0 {
        status += "!";
        if miss_percentage >= 0.5 {
            status += "!"
        }
    }




    return (result, status);
}


fn count_hits(rolls: &Vec<i32>) -> i32 {
    let mut count = 0;
    for i in 4..=6 {
        count += rolls[i]
    }
    
    return count
}

// fn prompt(name:&str) -> String {
//     let mut line = String::new();
//     print!("{}", name);
//     std::io::stdout().flush().unwrap();
//     std::io::stdin().read_line(&mut line).expect("Error: Could not read a line");

//     return line.trim().to_string()
// }

// fn main() {
//     let side: i32 = 6;
//     loop {
//         let num = prompt("Number of rolls OR exit: ");
//         if num.eq("exit") {
//             break;
//         }

//         if num.contains("*") {
//             let split_result: Vec<&str> = num.split("*").collect();
//             let mut r = split_result[0].parse().unwrap();
//             println!("{}/{}/{}\t{}", "h", "m", "tot", "note");
//             while r > 0 {
//                 let result = roll(r, side);
//                 simple_print(&result);
//                 r -= 1;
//             }

//         } else {
//             let r: i32 = num.parse().unwrap();
//             let result = roll(r, side);
//             simple_print(&result);
//         }
//     }
// }
