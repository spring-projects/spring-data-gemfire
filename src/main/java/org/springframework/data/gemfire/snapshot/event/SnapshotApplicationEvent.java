/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.snapshot.event;

import static org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata;

import org.apache.geode.cache.Region;
import org.springframework.context.ApplicationEvent;
import org.springframework.util.StringUtils;

/**
 * The SnapshotApplicationEvent class is a Spring ApplicationEvent signaling a GemFire Cache or Region snapshot event,
 * used to trigger a snapshot to occur.
 *
 * @author John Blum
 * @see org.springframework.context.ApplicationEvent
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public abstract class SnapshotApplicationEvent<K, V> extends ApplicationEvent {

	private final SnapshotMetadata<K, V>[] snapshotMetadata;

	private final String regionPath;

	/**
	 * Constructs an instance of SnapshotApplicationEvent initialized with an event source and optional meta-data
	 * describing the data snapshots to be imported/exported.
	 *
	 * @param source the source of the ApplicationEvent.
	 * @param snapshotMetadata an array of SnapshotMetadata containing details for each import/export.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public SnapshotApplicationEvent(Object source, SnapshotMetadata<K, V>... snapshotMetadata) {
		this(source, null, snapshotMetadata);
	}

	/**
	 * Constructs an instance of SnapshotApplicationEvent initialized with an event source, a pathname of the Region
	 * which data snapshots are imported/exported along with meta-data describing the details of the snapshot source.
	 *
	 * @param source the source of the ApplicationEvent.
	 * @param regionPath absolute pathname of the Region.
	 * @param snapshotMetadata an array of SnapshotMetadata containing details for each import/export.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public SnapshotApplicationEvent(Object source, String regionPath, SnapshotMetadata<K, V>... snapshotMetadata) {
		super(source);
		this.snapshotMetadata = snapshotMetadata;
		this.regionPath = regionPath;
	}

	/**
	 * Gets the absolute pathname of the Region in GemFire for which the snapshot will be taken.
	 *
	 * @return a String indicating the absolute pathname of the Region.
	 * @see org.apache.geode.cache.Region#getFullPath()
	 */
	public String getRegionPath() {
		return regionPath;
	}

	/**
	 * Gets the meta-data used to perform the GemFire Cache Region data snapshots.
	 *
	 * @return an array of SnapshotMetadata containing information necessary to perform the data export.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public SnapshotMetadata<K, V>[] getSnapshotMetadata() {
		return snapshotMetadata;
	}

	/**
	 * Determines whether this event indicates a Cache-wide snapshot.
	 *
	 * @return a boolean value indicating whether a Cache-wide snapshot has been triggered.
	 * @see #isRegionSnapshotEvent()
	 */
	public boolean isCacheSnapshotEvent() {
		return !isRegionSnapshotEvent();
	}

	/**
	 * Determines whether this event indicates a Region-specific snapshot.
	 *
	 * @return a boolean value indicating whether a Region-specific snapshot has been triggered.
	 * @see #isCacheSnapshotEvent()
	 */
	public boolean isRegionSnapshotEvent() {
		return StringUtils.hasText(getRegionPath());
	}

	/**
	 * Determines whether this event has been targeted for the specified Region.
	 *
	 * @param region the Region being evaluated as the subject of this event.
	 * @return a boolean value indicating whether this event has been targeted for the specified Region
	 * @see org.apache.geode.cache.Region#getFullPath()
	 * @see #getRegionPath()
	 * @see #matches(String)
	 */
	public boolean matches(Region region) {
		return (region != null && matches(region.getFullPath()));
	}

	/**
	 * Determines whether this event has been targeted for a Region with the given absolute pathname.
	 *
	 * @param regionPath the absolute Region pathname being evaluated as the subject of this event.
	 * @return a boolean value indicating whether this event has been targeted for the absolute Region pathname.
	 * @see #getRegionPath()
	 */
	public boolean matches(String regionPath) {
		return toString(regionPath).equals(toString(getRegionPath()));
	}

	/* (non-Javadoc) */
	private String toString(String value) {
		return String.valueOf(value).trim();
	}

}
