# python -m pip install kafka-python
# Reference: https://towardsdatascience.com/how-to-install-apache-kafka-using-docker-the-easy-way-4ceb00817d8b
# Reference: https://towardsdatascience.com/master-the-kafka-shell-in-5-minutes-topics-producers-and-consumers-explained-442a15f1dac1
# Reference: https://towardsdatascience.com/apache-kafka-in-python-how-to-stream-data-with-producers-and-consumers-307e84ca8bdb

# dummy data
import random

# data repository
temperatures = list(range(-10, 40))
humidities = list(range(1, 100))

# data callback
def generate_message() -> dict:
    random_temperature = random.choice(temperatures)
    random_humidity = random.choice(humidities)

    return {
        'temperature': random_temperature,
        'humidity': random_humidity,
        'timestamp': datetime.now().strftime("%d-%b-%Y (%H:%M:%S.%f)")
    }

# Producer
# kafka-topics.sh --create --zookeeper zookeeper:2181 --replication-factor 1 --partitions 1 --topic clima

import time 
import random 
from datetime import datetime
from kafka import KafkaProducer

# Messages will be serialized as JSON 
def serializer(message):
    return json.dumps(message).encode('utf-8')

# Kafka Producer
producer = KafkaProducer(
    bootstrap_servers=['localhost:9092'],
    value_serializer=serializer
)
if __name__ == '__main__':
    # Infinite loop - runs until you kill the program
    i = 0
    while i<5:
        # Generate a message
        dummy_message = generate_message()

        # Send it to our 'messages' topic
        print(f'Producing message @ {datetime.now()} | Message = {str(dummy_message)}')
        producer.send('clima', dummy_message)
        
        # Sleep for a random number of seconds
        time_to_sleep = random.randint(1, 11)
        time.sleep(time_to_sleep)
        i = i+1

# Consumer

from kafka import KafkaConsumer

if __name__ == '__main__':
    # Kafka Consumer 
    consumer = KafkaConsumer(
        'clima',
        bootstrap_servers='localhost:9092',
        auto_offset_reset='earliest'
    )

    for message in consumer:
        print(json.loads(message.value))