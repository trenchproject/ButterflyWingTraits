
!uv pip install opencv-python
!uv pip install scikit-image
!uv pip install scipy
!uv pip install pandas
!uv pip install matplotlib

from skimage import io, color, morphology, filters
from skimage.morphology import medial_axis
from scipy.ndimage import distance_transform_edt

#load packages
import cv2
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from PIL import Image

#load functions
def find_longest_axis(mask):
    # Ensure the mask is binary
    _, binary_mask = cv2.threshold(mask, 127, 255, cv2.THRESH_BINARY)
    
    # Find contours in the mask
    contours, _ = cv2.findContours(binary_mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    
    # Get the largest contour (assuming it's the wing)
    wing_contour = max(contours, key=cv2.contourArea)
    
    # Find the farthest pair of points within the contour
    longest_axis = 0
    p1, p2 = None, None
    
    for i in range(len(wing_contour)):
        for j in range(i + 1, len(wing_contour)):
            point1 = wing_contour[i][0]
            point2 = wing_contour[j][0]
            distance = np.linalg.norm(point1 - point2)
            if distance > longest_axis:
                longest_axis = distance
                p1, p2 = point1, point2
    
    return longest_axis, (p1, p2)

def plot_longest_axis(gray_image, mask):
    # Find the longest axis
    longest_axis, (p1, p2) = find_longest_axis(mask)
    
    # Create a color image for plotting
    color_image = cv2.cvtColor(gray_image, cv2.COLOR_GRAY2RGB)
    
    # Plot the image
    plt.figure(figsize=(10, 10))
    plt.imshow(color_image, cmap='gray')
    
    # Plot the mask outline
    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for contour in contours:
        plt.plot(contour[:, 0, 0], contour[:, 0, 1], color='b', linewidth=2)
    
    # Plot the longest axis
    plt.plot([p1[0], p2[0]], [p1[1], p2[1]], color='r', linewidth=2, label='Longest Axis')
    
    plt.title(f"Butterfly Wing - Longest Axis: {longest_axis:.2f} pixels")
    plt.legend()
    plt.axis('off')
    plt.show()

def place_measurement_circle(wing_mask, p1, p2, side='left', debug=False):
    """
    Places a measurement circle within a butterfly wing mask, positioned as close as possible
    to the specified side while ensuring it fits entirely within the wing mask.
    
    Parameters:
    -----------
    wing_mask : numpy.ndarray
        Binary mask of the butterfly wing (255 for wing, 0 for background)
    p1 : tuple (x, y)
        First endpoint of the longest axis
    p2 : tuple (x, y)
        Second endpoint of the longest axis
    side : str, optional
        Side where the body is located ('left' or 'right')
    debug : bool, optional
        If True, returns additional debug information
        
    Returns:
    --------
    tuple (center_x, center_y, radius) or None if no valid circle found
    If debug=True, also returns a visualization image
    """
    # Input validation
    if wing_mask is None or not isinstance(wing_mask, np.ndarray):
        raise ValueError("Wing mask must be a valid numpy array")
    
    if p1 is None or p2 is None or len(p1) != 2 or len(p2) != 2:
        raise ValueError("Endpoints p1 and p2 must be valid (x,y) coordinates")
    
    if side not in ['left', 'right']:
        raise ValueError("Side must be either 'left' or 'right'")
    
    # Convert points to numpy arrays for easier manipulation
    p1 = np.array(p1)
    p2 = np.array(p2)

    # Calculate the length of the longest axis
    axis_length = np.linalg.norm(p2 - p1)
    
    # Calculate circle diameter (25% of longest axis)
    circle_diameter = axis_length * 0.25
    radius = circle_diameter / 2
    
    # Create a copy of the mask for visualization if debug is True
    if debug:
        vis_img = cv2.cvtColor(wing_mask.copy(), cv2.COLOR_GRAY2BGR)
        # Draw the longest axis
        cv2.line(vis_img, tuple(map(int, p1)), tuple(map(int, p2)), (0, 255, 0), 2)
    
    # Determine which endpoint is closer to the specified side
    if side == 'left':
        if p1[0] < p2[0]:
            body_point = p1
            tip_point = p2
        else:
            body_point = p2
            tip_point = p1
    else:  # 'right'
        if p1[0] > p2[0]:
            body_point = p1
            tip_point = p2
        else:
            body_point = p2
            tip_point = p1
    
    # Create a distance transform of the wing mask
    # This will help us quickly find valid circle positions
    wing_mask_binary = wing_mask.copy()
    wing_mask_binary[wing_mask_binary > 0] = 1
    dist_transform = cv2.distanceTransform(wing_mask_binary, cv2.DIST_L2, 5)
    
    # Create a region of interest to search for circle positions
    # This is a rectangular area extending from the body point toward the tip
    direction = (tip_point - body_point) / np.linalg.norm(tip_point - body_point)

    # Determine search region dimensions
    search_length = axis_length * 0.5  # Search up to 50% of the axis length
    search_width = axis_length * 0.4   # Search width on each side of the axis
    
    # Create a grid of potential center points in the search region
    step_size = radius / 4  # Small steps for precision
    
    # Calculate perpendicular direction for search width
    perp_direction = np.array([-direction[1], direction[0]])
    
    # Initialize best center and distance
    best_center = None
    best_distance = float('inf')
    
    # Search grid for valid circle positions
    for t in np.arange(0, search_length, step_size):
        # Position along the axis
        axis_point = body_point + t * direction
        
        # Search perpendicular to the axis
        for w in np.arange(-search_width, search_width, step_size):
            # Potential center position
            potential_center = axis_point + w * perp_direction
            x, y = int(potential_center[0]), int(potential_center[1])
            
            # Skip if center is outside the image bounds
            if (x < 0 or y < 0 or 
                x >= wing_mask.shape[1] or 
                y >= wing_mask.shape[0]):
                continue
                
            # Check if this position allows a circle to fit within the wing
            # using the distance transform
            if dist_transform[y, x] >= radius:
                # Calculate distance to body point
                distance = np.linalg.norm(potential_center - body_point)
                
                # Check if this is closer to the body than our previous best
                if distance < best_distance:
                    best_distance = distance
                    best_center = (x, y)
    
    # If we couldn't find a valid position
    if best_center is None:
        if debug:
            return None, vis_img
        return None
    
    # Draw the final circle if in debug mode
    if debug:
        cv2.circle(vis_img, best_center, int(radius), (0, 0, 255), 2)
        # Draw the body point for reference
        cv2.circle(vis_img, tuple(map(int, body_point)), 5, (255, 0, 0), -1)
        return (best_center[0], best_center[1], int(radius)), vis_img
    
    return (best_center[0], best_center[1], int(radius)) 

def plot_measurement_circle(gray_image, mask, side):
    # Find the longest axis
    longest_axis, (p1, p2) = find_longest_axis(mask)

    # Position the measurement circle
    #circle_center = position_circle_cent(mask, longest_axis, p1, p2, side=side)
    #circle_center = find_measurement_center(mask, p1, p2, longest_axis, side=side)
    *circle_center,_ = place_measurement_circle(mask, p1, p2, side=side)

    # Create a color image for plotting
    color_image = cv2.cvtColor(gray_image, cv2.COLOR_GRAY2RGB)
    
    # Plot the image
    plt.figure(figsize=(10, 10))
    plt.imshow(color_image, cmap='gray')
    
    # Plot the mask outline
    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for contour in contours:
        plt.plot(contour[:, 0, 0], contour[:, 0, 1], color='b', linewidth=2)
    
    # Plot the longest axis
    plt.plot([p1[0], p2[0]], [p1[1], p2[1]], color='r', linewidth=2, label='Longest Axis')

    # Plot the measurement circle
    radius= longest_axis *0.25/2
    circle = plt.Circle(circle_center, radius, color='g', fill=False, linewidth=3, label='Measurement Circle')
    plt.gca().add_artist(circle)

    plt.title(f"Butterfly Wing - Longest Axis: {longest_axis:.2f} pixels")
    plt.legend()
    plt.axis('off')
    plt.show()

def estimate_grayscale_in_circle(image, center, radius):
    """
    Estimates the average grayscale intensity within a circular region.

    Parameters:
        image (numpy.ndarray): Input image (grayscale or color).
        center (tuple): Center of the circle (x, y).
        radius (int): Radius of the circle.

    Returns:
        float: Average grayscale intensity within the circle.
    """
    # Ensure the image is in grayscale
    if len(image.shape) == 3:  # If the image is in color
        gray_image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    else:
        gray_image = image

    # Create a mask for the circle
    mask = np.zeros_like(gray_image, dtype=np.uint8)
    cv2.circle(mask, center, radius, 255, -1)

   # Apply the mask to the grayscale image
    masked_image = cv2.bitwise_and(gray_image, gray_image, mask=mask)

    # Extract pixel values within the circle
    circle_pixels = masked_image[mask == 255]

    # Calculate and return average grayscale intensity
    if len(circle_pixels) > 0:
        return np.mean(circle_pixels)
    else:
        raise ValueError("Circle does not contain any valid pixels.")

def plot_measurement_circle_return(gray_image, mask, side):
    # Find the longest axis
    longest_axis, (p1, p2) = find_longest_axis(mask)

    # Position the measurement circle
    *circle_center, _ = place_measurement_circle(mask, p1, p2, side=side)

    # Create figure and axis objects
    fig, ax = plt.subplots(figsize=(10, 10))
    
    # Create color image and plot
    color_image = cv2.cvtColor(gray_image, cv2.COLOR_GRAY2RGB)
    ax.imshow(color_image, cmap='gray')
    
    # Plot mask outline
    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for contour in contours:
        ax.plot(contour[:, 0, 0], contour[:, 0, 1], color='b', linewidth=2)
    
    # Plot longest axis
    ax.plot([p1[0], p2[0]], [p1[1], p2[1]], color='r', linewidth=2, label='Longest Axis')

    # Plot measurement circle
    radius = longest_axis * 0.25 / 2
    circle = plt.Circle(circle_center, radius, color='g', 
                       fill=False, linewidth=3, label='Measurement Circle')
    ax.add_artist(circle)

    ax.set_title(f"Butterfly Wing - Longest Axis: {longest_axis:.2f} pixels")
    ax.legend()
    ax.axis('off')
    
    return fig, ax  # Return figure and axis objects

def process_images_with_masks(image_folder, output_folder):
    ## Create output folder if it doesn't exist
    #if not os.path.exists(output_folder):
    #    os.makedirs(output_folder)
    
    # Path to mask subfolder
    mask_folder = os.path.join(image_folder, "testimages_4masks/masks/")
    image_folder = os.path.join(image_folder, "testimages/")

    #desktop
    #mask_folder = os.path.join(image_folder, "masks/")
    #image_folder = os.path.join(image_folder, "testimages/")
    
    # Check if folders exist
    if not os.path.exists(image_folder):
        return f"Error: The folder '{image_folder}' does not exist."
    if not os.path.exists(mask_folder):
        return f"Error: The mask folder '{mask_folder}' does not exist."
    
    # Get all image files
    image_files = [f for f in os.listdir(image_folder) if f.endswith(('.png', '.jpg', '.jpeg')) 
                  and os.path.isfile(os.path.join(image_folder, f))]
    
    # Initialize metrics list
    bfdata = np.zeros((len(image_files),8))
        
    # Process each image
    for counter in range(0,len(image_files)):
        
        print(counter)
        file=image_files[counter]
   
        #make input paths
        image_path = os.path.join(image_folder, file)
        mask_rfw_path = os.path.join(mask_folder, file.rsplit('.jpg', 1)[0] + "_cls1.png")
        mask_lfw_path = os.path.join(mask_folder, file.rsplit('.jpg', 1)[0] + "_cls2.png")
        mask_rhw_path = os.path.join(mask_folder, file.rsplit('.jpg', 1)[0] + "_cls3.png")
        mask_lhw_path = os.path.join(mask_folder, file.rsplit('.jpg', 1)[0] + "_cls4.png")

        #make output paths
        output_path = os.path.join(output_folder, file)
        rfw_opath = os.path.join(output_folder, file + "_rfw.png")
        lfw_opath = os.path.join(output_folder, file + "_lfw.png")
        rhw_opath = os.path.join(output_folder, file + "_rhw.png")
        lhw_opath = os.path.join(output_folder, file + "_lhw.png")
        
        masks= [mask_rfw, mask_lfw, mask_rhw, mask_lhw]
        sides= ['left','right','left','right']
        mask_paths= [mask_rfw_path, mask_lfw_path, mask_rhw_path, mask_lhw_path]
        paths= [rfw_opath, lfw_opath, rhw_opath, lhw_opath]

        # Load image 
        image = cv2.imread(str(image_path))
        # Convert to gray scale
        image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        
        # Processing add error control
        for ind in [0,1,2,3]:

            # Skip if mask doesn't exist
            if not os.path.exists(mask_paths[ind]):
                print(f"Mask not found for {file}, skipping...")
                continue
        
            #read mask
            mask = cv2.imread(str(mask_paths[ind]))
            
            # Convert to gray scale
            mask = cv2.cvtColor(mask, cv2.COLOR_BGR2GRAY)
                    
            #find longest axis
            longest_axis, (p1, p2) = find_longest_axis(mask)
            
            #make a circle for grayscale measurement that has a diameter that is 25% of wing length
            center = place_measurement_circle(mask, p1, p2, side=sides[ind])

            #plot measurement circle
            fig, ax = plot_measurement_circle_return(image, mask, sides[ind])
            fig.savefig(paths[ind], bbox_inches='tight', dpi=300)
            plt.close(fig)  # Clean up memory
                
            #calculate grayscale
            gray= estimate_grayscale_in_circle(image, center[0:2], center[2])

            # Store data
            bfdata[counter,ind]=gray
            bfdata[counter,ind+4]=longest_axis

    # Convert to DataFrame
    bfdata_df = pd.DataFrame(bfdata)
    
    # Save metrics to CSV
    bfdata_df.to_csv(os.path.join(output_folder, 'bfdata.csv'), index=False)
       
    return bfdata_df

#==========================================
#RUN ANALYSIS

#Laptop
image_folder = '/Users/lbuckley/yolotemp/images/'  
output_folder = '/Users/lbuckley/yolotemp/images/out'

#Desktop
#image_folder = '/Users/laurenbuckley/wt_test/'  
#output_folder = '/Users/laurenbuckley/wt_test/out'

result= process_images_with_masks(image_folder, output_folder)

# Display results
if isinstance(result, pd.DataFrame):
    print(f"Processed {len(result)} images. Metrics saved to {output_folder}/bfdata.csv")
else:
    print(result)  # Print error message


    
            