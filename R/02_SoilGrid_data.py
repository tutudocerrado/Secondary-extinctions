import requests
import os

def download_soilgrid_raster(variable, depth, resolution, output_folder):
    url = f"https://rest.soilgrids.org/tiles/{variable}/{depth}/%d_%d.tif"
    
    for lat in range(10, -120, -31):
        for lon in range(-56, -35):
            response = requests.get(url % (lat, lon))
            if response.status_code == 200:
                filename = f"{variable}_{depth}m_{lat}_{lon}.tif"
                filepath = os.path.join(output_folder, filename)
                with open(filepath, 'wb') as f:
                    f.write(response.content)
                print(f"Baixado: {filename}")
            else:
                print(f"Falha ao baixar dados para lat={lat}, lon={lon}")

# Defina as variáveis e profundidades desejadas
variables = ['phh2o', 'ocd', 'sand', 'silt', 'clay', 'bdod', 'cec', 'cfvo', 'nitrogen', 'soc', 'ocs']
depths = ['0-5cm', '5-15cm', '15-30cm', '30-60cm', '60-100cm', '100-200cm']
resolution = 250  # Resolução em metros
output_folder = 'soilgrid_rasters'  # Pasta de saída para salvar os rasters

# Criar a pasta de saída se ela não existir
if not os.path.exists(output_folder):
    os.makedirs(output_folder)

# Baixar rasters para cada combinação de variável e profundidade
for variable in variables:
    for depth in depths:
        download_soilgrid_raster(variable, depth, resolution, output_folder)
        
        
