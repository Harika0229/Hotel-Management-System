# Define initial variables
rooms <- read.csv("data/rooms.csv")
reservations <- read.csv("data/reservations.csv")

# Function to display available rooms
display_available_rooms <- function() {
  available_rooms <- rooms[rooms$Status == "Available", ]
  reserved_rooms <- reservations$RoomNumber
  available_rooms <- available_rooms[!available_rooms$RoomNumber %in% reserved_rooms, ]
  if (nrow(available_rooms) == 0) {
    cat("No available rooms\n")
  } else {
    print(available_rooms)
  }
}

# Function to make a reservation
make_reservation <- function(room_number, guest_name, check_in, check_out, room_type) {
  if (room_number %in% reservations$RoomNumber) {
    cat("Room is already reserved\n")
  } else if (as.Date(check_out) <= as.Date(check_in)) {
    cat("Check-out date cannot be before check-in date\n")
  } else if (as.Date(check_in) < Sys.Date()) {
    cat("Check-in date cannot be a past date\n")
  } else {
    reservations <<- rbind(reservations, data.frame(RoomNumber = room_number, GuestName = guest_name, CheckIn = check_in, CheckOut = check_out, RoomType = room_type))
    rooms[rooms$RoomNumber == room_number, "Status"] <<- "Reserved"
    write.csv(rooms, "rooms.csv", row.names = FALSE)
    write.csv(reservations, "reservations.csv", row.names = FALSE)
    cat("Reservation made successfully\n")
  }
}

# Function to remove a reservation
remove_reservation <- function(room_number) {
  if (room_number %in% reservations$RoomNumber) {
    reservations <<- reservations[reservations$RoomNumber != room_number, ]
    rooms[rooms$RoomNumber == room_number, "Status"] <<- "Available"
    write.csv(rooms, "rooms.csv", row.names = FALSE)
    write.csv(reservations, "reservations.csv", row.names = FALSE)
    cat("Reservation removed successfully\n")
  } else {
    cat("Room is not reserved\n")
  }
}

# Function to display reservations
display_reservations <- function() {
  if (nrow(reservations) == 0) {
    cat("No reservations\n")
  } else {
    print(reservations)
  }
}

analyze_reservations_by_season <- function()
{
  # Convert CheckIn dates to Date objects
  reservations$CheckIn <- as.Date(reservations$CheckIn)
  
  # Create a new column 'Season' based on the month of CheckIn
  reservations$Season <- cut(reservations$CheckIn,
                             breaks = c(as.Date("2023-01-01"), as.Date("2023-04-01"),
                                        as.Date("2023-07-01"), as.Date("2023-10-01"), as.Date("2024-01-01")),
                             labels = c("Winter", "Spring", "Summer", "Fall"),
                             include.lowest = TRUE)
  
  # Perform statistical analysis by season
  season_summary <- aggregate(RoomNumber ~ Season, data = reservations, FUN = function(x) length(unique(x)))
  
  # Print the summary
  print(season_summary)
}

# Simulate interactions
while (TRUE) {
  cat("Options:\n1. Display available rooms\n2. Make a reservation\n3. Remove a reservation\n4. Display reservations\n5. Exit\n6.Analysis")
  choice <- as.integer(readline("Enter your choice: "))
  
  if (choice == 1) {
    display_available_rooms()
  } else if (choice == 2) {
    room_number <- as.integer(readline("Enter room number: "))
    guest_name <- readline("Enter guest name: ")
    check_in <- readline("Enter check-in date (YYYY-MM-DD): ")
    check_out <- readline("Enter check-out date (YYYY-MM-DD): ")
    room_type <- readline("Enter room type (1.AC single, 2.AC double, 3.Non-AC single, 4.Non-AC double): ")
    if (as.Date(check_out) <= as.Date(check_in)) {
      cat("Check-out date cannot be before check-in date\n")
    } else {
      make_reservation(room_number, guest_name, check_in, check_out, room_type)
    }
  } else if (choice == 3) {
    room_number <- as.integer(readline("Enter room number: "))
    remove_reservation(room_number)
  } else if (choice == 4) {
    display_reservations()
  } else if (choice == 5) {
    cat("Exiting\n")
    break
  }
  else if (choice == 6) {
    analyze_reservations_by_season()
  }
  
  
  else {
    cat("Invalid choice\n")
  }
}

