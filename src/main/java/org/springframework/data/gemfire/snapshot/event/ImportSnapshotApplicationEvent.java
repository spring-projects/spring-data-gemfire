/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.snapshot.event;

import static org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata;

/**
 * The ImportSnapshotApplicationEvent class is a Spring ApplicationEvent signaling a GemFire Cache or Region 'export'
 * snapshot event.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class ImportSnapshotApplicationEvent<K, V> extends SnapshotApplicationEvent<K, V> {

	/**
	 * Constructs an instance of ImportSnapshotApplicationEvent initialized with an event source and optional meta-data
	 * describing the data snapshots to be imported.
	 *
	 * @param source the source of the ApplicationEvent.
	 * @param snapshotMetadata an array of SnapshotMetadata containing details for each import.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public ImportSnapshotApplicationEvent(Object source, SnapshotMetadata<K, V>... snapshotMetadata) {
		super(source, snapshotMetadata);
	}

	/**
	 * Constructs an instance of ImportSnapshotApplicationEvent initialized with an event source, a pathname
	 * of the Region in which data is imported along with meta-data describing the details of the snapshot source.
	 *
	 * @param source the source of the ApplicationEvent.
	 * @param regionPath absolute pathname of the Region.
	 * @param snapshotMetadata an array of SnapshotMetadata containing details for each import/export.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public ImportSnapshotApplicationEvent(Object source, String regionPath, SnapshotMetadata<K, V>... snapshotMetadata) {
		super(source, regionPath, snapshotMetadata);
	}

}
