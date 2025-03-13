class Date:
    def __init__(self, year, month, day):
        """Initialize a Date object with year, month, and day."""
        self.year = year
        self.month = month
        self.day = day

    def __str__(self):
        """Format the date as YYYY-MM-DD."""
        return f"{self.year:04d}-{self.month:02d}-{self.day:02d}"
    
    def __eq__(self, other):
        """Check if two Date objects are equal."""
        if not isinstance(other, Date):
            return NotImplemented
        return (self.year, self.month, self.day) == (other.year, other.month, other.day)
    
    def __lt__(self, other):
        """Check if this Date is less than another Date."""
        if not isinstance(other, Date):
            return NotImplemented
        return (self.year, self.month, self.day) < (other.year, other.month, other.day)
    
    def __gt__(self, other):
        """Check if this Date is greater than another Date."""
        if not isinstance(other, Date):
            return NotImplemented
        return (self.year, self.month, self.day) > (other.year, other.month, other.day)
    
    def __le__(self, other):
        """Check if this Date is less than or equal to another Date."""
        if not isinstance(other, Date):
            return NotImplemented
        return (self.year, self.month, self.day) <= (other.year, other.month, other.day)
    
    def __ge__(self, other):
        """Check if this Date is greater than or equal to another Date."""
        if not isinstance(other, Date):
            return NotImplemented
        return (self.year, self.month, self.day) >= (other.year, other.month, other.day)