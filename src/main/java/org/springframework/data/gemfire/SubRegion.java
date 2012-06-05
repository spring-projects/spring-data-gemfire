package org.springframework.data.gemfire;

import java.util.List;

import com.gemstone.gemfire.cache.RegionAttributes;
/**
 * 
 * @author David Turanski
 *
 * @param <K>
 * @param <V>
 */
public class SubRegion<K, V> {
	private final RegionAttributes<K, V> regionAttributes;
	private final String regionName;
	private List<SubRegion<?,?>> subRegions;
	
	public SubRegion(String regionName, RegionAttributes<K, V> regionAttributes) {
		this.regionAttributes = regionAttributes;
		this.regionName = regionName;
	}
	
	public RegionAttributes<K, V> getRegionAttributes() {
		return regionAttributes;
	}
	 
	public String getRegionName() {
		return regionName;
	}
	
	public List<SubRegion<?, ?>> getSubRegions() {
		return subRegions;
	}
	public void setSubRegions(List<SubRegion<?, ?>> subRegions) {
		this.subRegions = subRegions;
	} 
}
