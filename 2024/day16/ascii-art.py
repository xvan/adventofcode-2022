from PIL import Image, ImageDraw, ImageFont
import os

# Set up parameters
ascii_dir = "mapas"  # Folder containing ASCII files
output_dir = "image_frames"  # Folder to save image frames
font_path = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"  # Path to a monospaced font
font_size = 20  # Adjust as needed; try starting with 20 for clarity
char_width = 10  # Approximate width of each character in pixels
char_height = 20  # Approximate height of each character in pixels
num_columns = 24  # Number of characters per line
num_rows = 11  # Number of lines
text_color = (255, 255, 255)  # White text
background_color = (0, 0, 0)  # Black background

# Calculate image size
image_width = char_width * num_columns
image_height = char_height * num_rows
image_size = (image_width, image_height)

# Create output directory if it doesn't exist
os.makedirs(output_dir, exist_ok=True)

# Process each ASCII file
for filename in sorted(os.listdir(ascii_dir)):
    if filename.endswith(".txt"):
        with open(os.path.join(ascii_dir, filename), "r") as f:
            ascii_content = f.read()

        # Create an image
        img = Image.new("RGB", image_size, background_color)
        draw = ImageDraw.Draw(img)
        font = ImageFont.truetype(font_path, font_size)

        # Draw text
        draw.multiline_text((0, 0), ascii_content, font=font, fill=text_color, spacing=0)

        # Save the image
        output_path = os.path.join(output_dir, f"{filename[:-4]}.png")
        img.save(output_path)

